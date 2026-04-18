using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

namespace Macaron.InterfaceDelegation;

public static class MemberComparisonHelpers
{
    public static bool HasCompatibleImplementation(
        ITypeSymbol typeSymbol,
        ITypeSymbol interfaceSymbol,
        ISymbol interfaceMemberSymbol
    )
    {
        var symbolComparer = SymbolEqualityComparer.Default;

        foreach (var memberSymbol in GetComparableMembers(typeSymbol))
        {
            switch (interfaceMemberSymbol)
            {
                case IMethodSymbol methodSymbol when memberSymbol is IMethodSymbol targetMethodSymbol:
                    if (targetMethodSymbol is
                        {
                            MethodKind: MethodKind.ExplicitInterfaceImplementation,
                            ExplicitInterfaceImplementations: var explicitImplementations,
                        } &&
                        explicitImplementations.Any(explicitSymbol => symbolComparer.Equals(explicitSymbol, methodSymbol)))
                    {
                        return true;
                    }

                    if (targetMethodSymbol.MethodKind == MethodKind.Ordinary &&
                        MatchesMethodSignature(methodSymbol, methodSymbol.Name, targetMethodSymbol, checkReturnType: true))
                    {
                        return true;
                    }
                    break;
                case IPropertySymbol propertySymbol when memberSymbol is IPropertySymbol targetPropertySymbol:
                    if (targetPropertySymbol.ExplicitInterfaceImplementations.Any(explicitSymbol => symbolComparer.Equals(explicitSymbol, propertySymbol)))
                    {
                        return true;
                    }

                    if (MatchesPropertySignature(propertySymbol, propertySymbol.Name, targetPropertySymbol))
                    {
                        return true;
                    }
                    break;
                case IEventSymbol eventSymbol when memberSymbol is IEventSymbol targetEventSymbol:
                    if (targetEventSymbol.ExplicitInterfaceImplementations.Any(explicitSymbol => symbolComparer.Equals(explicitSymbol, eventSymbol)))
                    {
                        return true;
                    }

                    if (MatchesEventSignature(eventSymbol, eventSymbol.Name, targetEventSymbol))
                    {
                        return true;
                    }
                    break;
            }
        }

        return false;
    }

    public static Func<ISymbol, string, bool, bool, ISymbol?> BuildMemberComparer(
        ITypeSymbol typeSymbol,
        ITypeSymbol interfaceSymbol
    )
    {
        var symbolComparer = SymbolEqualityComparer.Default;

        var methodSymbols = new List<IMethodSymbol>();
        var propertySymbols = new List<IPropertySymbol>();
        var eventSymbols = new List<IEventSymbol>();
        var explicitMethodSymbols = new List<IMethodSymbol>();
        var explicitPropertySymbols = new List<IPropertySymbol>();
        var explicitEventSymbols = new List<IEventSymbol>();

        foreach (var memberSymbol in GetMembersWithBaseTypes(typeSymbol))
        {
            if (memberSymbol is IMethodSymbol { MethodKind: not MethodKind.Constructor } methodSymbol)
            {
                if (methodSymbol is
                {
                    MethodKind: MethodKind.ExplicitInterfaceImplementation,
                    ExplicitInterfaceImplementations: [var explicitMethodSymbol],
                })
                {
                    if (symbolComparer.Equals(explicitMethodSymbol.ContainingType, interfaceSymbol))
                    {
                        explicitMethodSymbols.Add(explicitMethodSymbol);
                    }
                }
                else if (methodSymbol.MethodKind == MethodKind.Ordinary)
                {
                    methodSymbols.Add(methodSymbol);
                }
            }
            else if (memberSymbol is IPropertySymbol propertySymbol)
            {
                if (propertySymbol.ExplicitInterfaceImplementations is [var explicitPropertySymbol])
                {
                    if (symbolComparer.Equals(explicitPropertySymbol.ContainingType, interfaceSymbol))
                    {
                        explicitPropertySymbols.Add(explicitPropertySymbol);
                    }
                }
                else
                {
                    propertySymbols.Add(propertySymbol);
                }
            }
            else if (memberSymbol is IEventSymbol eventSymbol)
            {
                if (eventSymbol.ExplicitInterfaceImplementations is [var explicitEventSymbol])
                {
                    if (symbolComparer.Equals(explicitEventSymbol.ContainingType, interfaceSymbol))
                    {
                        explicitEventSymbols.Add(explicitEventSymbol);
                    }
                }
                else
                {
                    eventSymbols.Add(eventSymbol);
                }
            }
        }

        var methodSymbolsDict = ToDictionary(methodSymbols, methodSymbol => methodSymbol.Name);
        var explicitMethodSymbolsDict = ToDictionary(explicitMethodSymbols, methodSymbol => methodSymbol.Name);
        var propertySymbolsDict = ToDictionary(propertySymbols, propertySymbol => propertySymbol.Name);
        var explicitPropertySymbolsDict = ToDictionary(explicitPropertySymbols, propertySymbol => propertySymbol.Name);
        var eventSymbolsDict = ToDictionary(eventSymbols, eventSymbol => eventSymbol.Name);
        var explicitEventSymbolsDict = ToDictionary(explicitEventSymbols, eventSymbol => eventSymbol.Name);

        return (symbol, symbolName, isExplicit, checkReturnType) =>
        {
            if (symbol is IMethodSymbol methodSymbol)
            {
                var dict = isExplicit ? explicitMethodSymbolsDict : methodSymbolsDict;
                if (dict.TryGetValue(symbolName, out var symbols))
                {
                    return symbols.FirstOrDefault(methodSymbol2 =>
                    {
                        return MatchesMethodSignature(methodSymbol, symbolName, methodSymbol2, checkReturnType);
                    });
                }
            }
            else if (symbol is IPropertySymbol propertySymbol)
            {
                var dict = isExplicit ? explicitPropertySymbolsDict : propertySymbolsDict;
                if (dict.TryGetValue(symbolName, out var symbols))
                {
                    return symbols.FirstOrDefault(propertySymbol2 =>
                    {
                        return MatchesPropertySignature(propertySymbol, symbolName, propertySymbol2);
                    });
                }
            }
            else if (symbol is IEventSymbol eventSymbol)
            {
                var dict = isExplicit ? explicitEventSymbolsDict : eventSymbolsDict;
                if (dict.TryGetValue(symbolName, out var symbols))
                {
                    return symbols.FirstOrDefault(eventSymbol2 =>
                    {
                        return MatchesEventSignature(eventSymbol, symbolName, eventSymbol2);
                    });
                }
            }

            return null;
        };

        #region Local Functions
        static ImmutableDictionary<string, ImmutableArray<T>> ToDictionary<T>(IEnumerable<T> symbols, Func<T, string> getKey)
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
