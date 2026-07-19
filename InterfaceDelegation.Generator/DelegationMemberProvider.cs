using Microsoft.CodeAnalysis;

using static Microsoft.CodeAnalysis.SymbolDisplayFormat;
using static Microsoft.CodeAnalysis.TypeKind;

namespace Macaron.InterfaceDelegation;

internal static class DelegationMemberProvider
{
    public static IEnumerable<ISymbol> GetDeclaredMembers(ITypeSymbol typeSymbol)
    {
        foreach (var memberSymbol in typeSymbol.GetMembers())
        {
            if (!memberSymbol.IsStatic)
            {
                yield return memberSymbol;
            }
        }
    }

    public static IEnumerable<ISymbol> GetMembersIncludingBaseTypes(ITypeSymbol typeSymbol)
    {
        if (typeSymbol.TypeKind == Interface)
        {
            foreach (var memberSymbol in new[] { typeSymbol }
                .Concat(typeSymbol.AllInterfaces)
                .SelectMany(GetDeclaredMembers)
            )
            {
                yield return memberSymbol;
            }

            yield break;
        }

        var overriddenSymbols = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
        var baseTypeSymbol = typeSymbol;

        while (baseTypeSymbol != null && !IsRootType(baseTypeSymbol))
        {
            foreach (var memberSymbol in baseTypeSymbol.GetMembers())
            {
                if (memberSymbol.IsStatic)
                {
                    continue;
                }

                switch (memberSymbol)
                {
                    case IMethodSymbol { OverriddenMethod: { } overriddenMethod }:
                    {
                        overriddenSymbols.Add(overriddenMethod);

                        break;
                    }
                    case IPropertySymbol { OverriddenProperty: { } overriddenProperty }:
                    {
                        overriddenSymbols.Add(overriddenProperty);

                        break;
                    }
                    case IEventSymbol { OverriddenEvent: { } overriddenEvent }:
                    {
                        overriddenSymbols.Add(overriddenEvent);

                        break;
                    }
                }

                if (overriddenSymbols.Contains(memberSymbol))
                {
                    continue;
                }

                yield return memberSymbol;
            }

            baseTypeSymbol = baseTypeSymbol.BaseType;
        }
    }

    private static bool IsRootType(ITypeSymbol symbol)
    {
        return symbol.ToDisplayString(FullyQualifiedFormat) is "object" or "global::System.ValueType";
    }
}
