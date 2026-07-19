using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

namespace Macaron.InterfaceDelegation;

public static class MemberComparisonHelper
{
    private readonly record struct MethodLookupKey(
        string Name,
        int Arity,
        int ParameterCount,
        int ParameterHash
    );

    private readonly record struct PropertyLookupKey(
        string Name,
        int ParameterCount,
        int SignatureHash
    );

    private readonly record struct EventLookupKey(
        string Name,
        int TypeHash
    );

    public static bool ImplementsInterface(ITypeSymbol typeSymbol, ITypeSymbol interfaceSymbol)
    {
        var comparer = SymbolEqualityComparer.Default;
        return comparer.Equals(typeSymbol, interfaceSymbol) ||
            typeSymbol.AllInterfaces.Contains(interfaceSymbol, comparer);
    }

    public static bool HasCompatibleImplementation(
        ITypeSymbol typeSymbol,
        ITypeSymbol interfaceSymbol,
        ISymbol interfaceMemberSymbol
    )
    {
        return BuildCompatibleImplementationChecker(typeSymbol, interfaceSymbol)(interfaceMemberSymbol);
    }

    public static Func<ISymbol, bool> BuildCompatibleImplementationChecker(
        ITypeSymbol typeSymbol,
        ITypeSymbol interfaceSymbol,
        Func<ISymbol, bool>? isAccessible = null
    )
    {
        var memberSymbols = GetComparableMembers(typeSymbol);

        if (isAccessible != null)
        {
            memberSymbols = memberSymbols.Where(isAccessible);
        }

        var explicitImplementations = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
        var getImplementedMember = BuildMemberComparerCore(
            memberSymbols,
            interfaceSymbol,
            explicitImplementations
        );

        return interfaceMemberSymbol =>
            getImplementedMember(interfaceMemberSymbol, interfaceMemberSymbol.Name, false, true) != null
            || explicitImplementations.Contains(interfaceMemberSymbol);
    }

    public static Func<ISymbol, string, bool, bool, ISymbol?> BuildMemberComparer(
        ITypeSymbol typeSymbol,
        ITypeSymbol interfaceSymbol
    )
    {
        return BuildMemberComparerCore(GetMembersWithBaseTypes(typeSymbol), interfaceSymbol);
    }

    private static Func<ISymbol, string, bool, bool, ISymbol?> BuildMemberComparerCore(
        IEnumerable<ISymbol> memberSymbols,
        ITypeSymbol interfaceSymbol,
        HashSet<ISymbol>? explicitImplementations = null
    )
    {
        var symbolComparer = SymbolEqualityComparer.Default;

        var methodSymbols = new List<IMethodSymbol>();
        var propertySymbols = new List<IPropertySymbol>();
        var eventSymbols = new List<IEventSymbol>();
        var explicitMethodSymbols = new List<IMethodSymbol>();
        var explicitPropertySymbols = new List<IPropertySymbol>();
        var explicitEventSymbols = new List<IEventSymbol>();

        foreach (var memberSymbol in memberSymbols)
        {
            switch (memberSymbol)
            {
                case IMethodSymbol
                {
                    MethodKind: not MethodKind.Constructor,
                }
                and
                {
                    MethodKind: MethodKind.ExplicitInterfaceImplementation,
                    ExplicitInterfaceImplementations: var explicitMethodSymbolsForMember,
                }:
                {
                    foreach (var explicitMethodSymbol in explicitMethodSymbolsForMember)
                    {
                        explicitImplementations?.Add(explicitMethodSymbol);

                        if (symbolComparer.Equals(explicitMethodSymbol.ContainingType, interfaceSymbol))
                        {
                            explicitMethodSymbols.Add(explicitMethodSymbol);
                        }
                    }

                    break;
                }
                case IMethodSymbol { MethodKind: not MethodKind.Constructor } methodSymbol:
                {
                    if (methodSymbol.MethodKind == MethodKind.Ordinary)
                    {
                        methodSymbols.Add(methodSymbol);
                    }

                    break;
                }
                case IPropertySymbol { ExplicitInterfaceImplementations.IsEmpty: false } propertySymbol:
                {
                    foreach (var explicitPropertySymbol in propertySymbol.ExplicitInterfaceImplementations)
                    {
                        explicitImplementations?.Add(explicitPropertySymbol);

                        if (symbolComparer.Equals(explicitPropertySymbol.ContainingType, interfaceSymbol))
                        {
                            explicitPropertySymbols.Add(explicitPropertySymbol);
                        }
                    }

                    break;
                }
                case IPropertySymbol propertySymbol:
                {
                    propertySymbols.Add(propertySymbol);

                    break;
                }
                case IEventSymbol { ExplicitInterfaceImplementations.IsEmpty: false } eventSymbol:
                {
                    foreach (var explicitEventSymbol in eventSymbol.ExplicitInterfaceImplementations)
                    {
                        explicitImplementations?.Add(explicitEventSymbol);

                        if (symbolComparer.Equals(explicitEventSymbol.ContainingType, interfaceSymbol))
                        {
                            explicitEventSymbols.Add(explicitEventSymbol);
                        }
                    }

                    break;
                }
                case IEventSymbol eventSymbol:
                {
                    eventSymbols.Add(eventSymbol);

                    break;
                }
            }
        }

        var methodSymbolsDict = ToDictionary(methodSymbols, methodSymbol => GetMethodLookupKey(methodSymbol, methodSymbol.Name));
        var explicitMethodSymbolsDict = ToDictionary(explicitMethodSymbols, methodSymbol => GetMethodLookupKey(methodSymbol, methodSymbol.Name));
        var propertySymbolsDict = ToDictionary(propertySymbols, propertySymbol => GetPropertyLookupKey(propertySymbol, propertySymbol.Name));
        var explicitPropertySymbolsDict = ToDictionary(explicitPropertySymbols, propertySymbol => GetPropertyLookupKey(propertySymbol, propertySymbol.Name));
        var eventSymbolsDict = ToDictionary(eventSymbols, eventSymbol => GetEventLookupKey(eventSymbol, eventSymbol.Name));
        var explicitEventSymbolsDict = ToDictionary(explicitEventSymbols, eventSymbol => GetEventLookupKey(eventSymbol, eventSymbol.Name));

        return (symbol, symbolName, isExplicit, checkReturnType) =>
        {
            switch (symbol)
            {
                case IMethodSymbol methodSymbol:
                {
                    var dict = isExplicit ? explicitMethodSymbolsDict : methodSymbolsDict;

                    if (dict.TryGetValue(GetMethodLookupKey(methodSymbol, symbolName), out var symbols))
                    {
                        return symbols.FirstOrDefault(methodSymbol2 =>
                        {
                            return MatchesMethodSignature(methodSymbol, symbolName, methodSymbol2, checkReturnType);
                        });
                    }

                    break;
                }
                case IPropertySymbol propertySymbol:
                {
                    var dict = isExplicit ? explicitPropertySymbolsDict : propertySymbolsDict;

                    if (dict.TryGetValue(GetPropertyLookupKey(propertySymbol, symbolName), out var symbols))
                    {
                        return symbols.FirstOrDefault(propertySymbol2 =>
                        {
                            return MatchesPropertySignature(propertySymbol, symbolName, propertySymbol2);
                        });
                    }

                    break;
                }
                case IEventSymbol eventSymbol:
                {
                    var dict = isExplicit ? explicitEventSymbolsDict : eventSymbolsDict;

                    if (dict.TryGetValue(GetEventLookupKey(eventSymbol, symbolName), out var symbols))
                    {
                        return symbols.FirstOrDefault(eventSymbol2 =>
                        {
                            return MatchesEventSignature(eventSymbol, symbolName, eventSymbol2);
                        });
                    }

                    break;
                }
            }

            return null;
        };

        #region Local Functions
        static ImmutableDictionary<TKey, ImmutableArray<T>> ToDictionary<TKey, T>(
            IEnumerable<T> symbols,
            Func<T, TKey> getKey
        ) where TKey : notnull
        {
            return symbols
                .GroupBy(getKey)
                .ToImmutableDictionary(
                    keySelector: grouping => grouping.Key,
                    elementSelector: grouping => grouping.ToImmutableArray()
                );
        }
        #endregion
    }

    private static MethodLookupKey GetMethodLookupKey(IMethodSymbol methodSymbol, string methodName)
    {
        var parameterHash = 17;
        foreach (var parameterSymbol in methodSymbol.Parameters)
        {
            parameterHash = CombineHash(parameterHash, SymbolEqualityComparer.Default.GetHashCode(parameterSymbol.Type));
            parameterHash = CombineHash(parameterHash, (int)parameterSymbol.RefKind);
            parameterHash = CombineHash(parameterHash, parameterSymbol.IsParams ? 1 : 0);
        }

        return new MethodLookupKey(
            methodName,
            methodSymbol.Arity,
            methodSymbol.Parameters.Length,
            parameterHash
        );
    }

    private static PropertyLookupKey GetPropertyLookupKey(IPropertySymbol propertySymbol, string propertyName)
    {
        var signatureHash = SymbolEqualityComparer.Default.GetHashCode(propertySymbol.Type);
        foreach (var parameterSymbol in propertySymbol.Parameters)
        {
            signatureHash = CombineHash(signatureHash, SymbolEqualityComparer.Default.GetHashCode(parameterSymbol.Type));
            signatureHash = CombineHash(signatureHash, (int)parameterSymbol.RefKind);
        }

        return new PropertyLookupKey(propertyName, propertySymbol.Parameters.Length, signatureHash);
    }

    private static EventLookupKey GetEventLookupKey(IEventSymbol eventSymbol, string eventName)
    {
        return new EventLookupKey(eventName, SymbolEqualityComparer.Default.GetHashCode(eventSymbol.Type));
    }

    private static int CombineHash(int current, int value)
    {
        return unchecked((current * 31) + value);
    }

    private static IEnumerable<ISymbol> GetMembersWithBaseTypes(ITypeSymbol typeSymbol)
    {
        foreach (var memberSymbol in typeSymbol.GetMembers())
        {
            yield return memberSymbol;
        }

        var baseTypeSymbol = typeSymbol.BaseType;
        while (baseTypeSymbol != null && !IsSystemType(baseTypeSymbol))
        {
            foreach (var memberSymbol in baseTypeSymbol
                .GetMembers()
                .Where(symbol => symbol.DeclaredAccessibility != Accessibility.Private)
            )
            {
                yield return memberSymbol;
            }

            baseTypeSymbol = baseTypeSymbol.BaseType;
        }

        #region Local Functions
        static bool IsSystemType(ITypeSymbol symbol)
        {
            return symbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat) is "object" or "global::System.ValueType";
        }
        #endregion
    }

    internal static bool MatchesMethodSignature(
        IMethodSymbol methodSymbol,
        string methodName,
        IMethodSymbol targetMethodSymbol,
        bool checkReturnType
    )
    {
        var comparer = SymbolEqualityComparer.Default;

        if (methodName != targetMethodSymbol.Name)
        {
            return false;
        }

        if (checkReturnType && !comparer.Equals(methodSymbol.ReturnType, targetMethodSymbol.ReturnType))
        {
            return false;
        }

        if (methodSymbol.Parameters.Length != targetMethodSymbol.Parameters.Length)
        {
            return false;
        }

        if (methodSymbol.Arity != targetMethodSymbol.Arity)
        {
            return false;
        }

        for (var i = 0; i < methodSymbol.Parameters.Length; i++)
        {
            var paramSymbol = methodSymbol.Parameters[i];
            var targetParamSymbol = targetMethodSymbol.Parameters[i];

            if (!comparer.Equals(paramSymbol.Type, targetParamSymbol.Type))
            {
                return false;
            }

            if (paramSymbol.RefKind != targetParamSymbol.RefKind)
            {
                return false;
            }

            if (paramSymbol.IsParams != targetParamSymbol.IsParams)
            {
                return false;
            }
        }

        return true;
    }

    internal static bool MatchesPropertySignature(
        IPropertySymbol propertySymbol,
        string propertyName,
        IPropertySymbol targetPropertySymbol
    )
    {
        var comparer = SymbolEqualityComparer.Default;

        if (!propertyName.Equals(targetPropertySymbol.Name) ||
            !comparer.Equals(propertySymbol.Type, targetPropertySymbol.Type) ||
            propertySymbol.Parameters.Length != targetPropertySymbol.Parameters.Length)
        {
            return false;
        }

        for (var i = 0; i < propertySymbol.Parameters.Length; i++)
        {
            var paramSymbol = propertySymbol.Parameters[i];
            var targetParamSymbol = targetPropertySymbol.Parameters[i];

            if (!comparer.Equals(paramSymbol.Type, targetParamSymbol.Type) ||
                paramSymbol.RefKind != targetParamSymbol.RefKind)
            {
                return false;
            }
        }

        if (propertySymbol.GetMethod != null && targetPropertySymbol.GetMethod == null)
        {
            return false;
        }

        return propertySymbol.SetMethod == null || targetPropertySymbol.SetMethod != null;
    }

    internal static bool MatchesEventSignature(
        IEventSymbol eventSymbol,
        string eventName,
        IEventSymbol targetEventSymbol
    )
    {
        var comparer = SymbolEqualityComparer.Default;

        return
            eventName.Equals(targetEventSymbol.Name) &&
            comparer.Equals(eventSymbol.Type, targetEventSymbol.Type);
    }

    private static IEnumerable<ISymbol> GetComparableMembers(ITypeSymbol typeSymbol)
    {
        if (typeSymbol.TypeKind == TypeKind.Interface)
        {
            foreach (var memberSymbol in DelegationMemberUtilities.GetMembersWithBaseTypes(typeSymbol))
            {
                if (!memberSymbol.IsStatic)
                {
                    yield return memberSymbol;
                }
            }

            yield break;
        }

        foreach (var memberSymbol in GetMembersWithBaseTypes(typeSymbol))
        {
            if (!memberSymbol.IsStatic)
            {
                yield return memberSymbol;
            }
        }
    }
}
