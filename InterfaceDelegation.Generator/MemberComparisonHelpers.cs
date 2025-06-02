using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

namespace Macaron.InterfaceDelegation;

public static class MemberComparisonHelpers
{
    public static Func<ISymbol, string, bool, bool, ISymbol?> BuildMemberComparer(
        ITypeSymbol typeSymbol,
        ITypeSymbol interfaceSymbol
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

    private static bool MatchesMethodSignature(
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

    private static bool MatchesPropertySignature(
        IPropertySymbol propertySymbol,
        string propertyName,
        IPropertySymbol targetPropertySymbol
    )
    {
        var comparer = SymbolEqualityComparer.Default;
        return
            propertyName.Equals(targetPropertySymbol.Name) &&
            comparer.Equals(propertySymbol.Type, targetPropertySymbol.Type);
    }
}
