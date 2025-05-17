using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

namespace Macaron.InterfaceDelegation;

public static class MemberComparisonHelpers
{
    public static Func<ISymbol, bool, ISymbol?> BuildMemberComparer(
        INamedTypeSymbol typeSymbol,
        INamedTypeSymbol interfaceSymbol
    )
    {
        var symbolComparer = SymbolEqualityComparer.Default;

        var methodSymbols = new List<IMethodSymbol>();
        var propertySymbols = new List<IPropertySymbol>();
        var explicitMethodSymbols = new List<IMethodSymbol>();
        var explicitPropertySymbols = new List<IPropertySymbol>();

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
        }

        var methodSymbolsDict = ToDictionary(methodSymbols, methodSymbol => methodSymbol.Name);
        var explicitMethodSymbolsDict = ToDictionary(explicitMethodSymbols, methodSymbol => methodSymbol.Name);
        var propertySymbolsDict = ToDictionary(propertySymbols, propertySymbol => propertySymbol.Name);
        var explicitPropertySymbolsDict = ToDictionary(explicitPropertySymbols, propertySymbol => propertySymbol.Name);

        return (symbol, isExplicit) =>
        {
            if (symbol is IMethodSymbol methodSymbol)
            {
                var dict = isExplicit ? explicitMethodSymbolsDict : methodSymbolsDict;
                if (dict.TryGetValue(methodSymbol.Name, out var symbols))
                {
                    return symbols.FirstOrDefault(methodSymbol2 =>
                    {
                        return MatchesMethodSignature(methodSymbol2, methodSymbol);
                    });
                }
            }
            else if (symbol is IPropertySymbol propertySymbol)
            {
                var dict = isExplicit ? explicitPropertySymbolsDict : propertySymbolsDict;
                if (dict.TryGetValue(propertySymbol.Name, out var symbols))
                {
                    return symbols.FirstOrDefault(propertySymbol2 =>
                    {
                        return MatchesPropertySignature(propertySymbol2, propertySymbol);
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

    private static IEnumerable<ISymbol> GetMembersWithBaseTypes(INamedTypeSymbol typeSymbol)
    {
        foreach (var memberSymbol in typeSymbol.GetMembers())
        {
            yield return memberSymbol;
        }

        var baseTypeSymbol = typeSymbol.BaseType;
        while (baseTypeSymbol != null)
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
    }

    private static bool MatchesMethodSignature(IMethodSymbol method1, IMethodSymbol method2)
    {
        var comparer = SymbolEqualityComparer.Default;

        if (method1.Name != method2.Name)
        {
            return false;
        }

        if (!comparer.Equals(method1.ReturnType, method2.ReturnType))
        {
            return false;
        }

        if (method1.Parameters.Length != method2.Parameters.Length)
        {
            return false;
        }

        for (var i = 0; i < method1.Parameters.Length; i++)
        {
            var paramSymbol1 = method1.Parameters[i];
            var paramSymbol2 = method2.Parameters[i];

            if (!comparer.Equals(paramSymbol1.Type, paramSymbol2.Type))
            {
                return false;
            }

            if (paramSymbol1.RefKind != paramSymbol2.RefKind)
            {
                return false;
            }

            if (paramSymbol1.IsParams != paramSymbol2.IsParams)
            {
                return false;
            }
        }

        return true;
    }

    private static bool MatchesPropertySignature(IPropertySymbol property1, IPropertySymbol property2)
    {
        var comparer = SymbolEqualityComparer.Default;
        return property1.Name == property2.Name && comparer.Equals(property1.Type, property2.Type);
    }
}
