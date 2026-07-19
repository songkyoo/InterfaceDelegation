using Microsoft.CodeAnalysis;
using static Microsoft.CodeAnalysis.SymbolDisplayFormat;
using static Microsoft.CodeAnalysis.TypeKind;

namespace Macaron.InterfaceDelegation;

internal static class DelegationMemberUtilities
{
    internal enum MemberGenerationMode
    {
        ImplicitInterfaceImplementation,
        ExplicitInterfaceImplementation,
        Lift,
    }

    internal readonly record struct MemberGenerationContext(
        ISymbol Symbol,
        string SymbolName,
        bool IsAbstract,
        string Accessibility,
        string InterfacePrefix
    );

    public static IEnumerable<ISymbol> GetMembersWithBaseTypes(ITypeSymbol typeSymbol)
    {
        if (typeSymbol.TypeKind == Interface)
        {
            foreach (var memberSymbol in new[] { typeSymbol }.Concat(typeSymbol.AllInterfaces)
                .SelectMany(symbol => symbol.GetMembers())
                .Where(symbol => !symbol.IsStatic)
            )
            {
                yield return memberSymbol;
            }

            yield break;
        }

        var overriddenSymbols = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
        var baseTypeSymbol = typeSymbol;

        while (baseTypeSymbol != null && !IsBaseType(baseTypeSymbol))
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

    public static IEnumerable<ISymbol> GetMembers(ITypeSymbol typeSymbol)
    {
        foreach (var memberSymbol in typeSymbol.GetMembers())
        {
            if (!memberSymbol.IsStatic)
            {
                yield return memberSymbol;
            }
        }
    }

    public static (bool hasImplementedMember, bool isExplicit, bool isAbstract) GetImplementationContext(
        MemberGenerationMode mode,
        ITypeSymbol? containingTypeSymbol,
        ISymbol? implicitMemberSymbol,
        ISymbol? explicitMemberSymbol
    )
    {
        var defaultValue = (
            hasImplementedMember: false,
            isExplicit: false,
            isAbstract: false
        );

        var result = mode switch
        {
            MemberGenerationMode.ImplicitInterfaceImplementation => (
                implicitMemberSymbol,
                explicitMemberSymbol
            ) switch
            {
                (null, null) => defaultValue,
                ({ IsAbstract: true }, null) => defaultValue with { isAbstract = true },
                _ => defaultValue with { hasImplementedMember = true },
            },
            MemberGenerationMode.ExplicitInterfaceImplementation => explicitMemberSymbol == null
                ? defaultValue with { isExplicit = true }
                : defaultValue with { hasImplementedMember = true },
            MemberGenerationMode.Lift => implicitMemberSymbol switch
            {
                null => defaultValue,
                { IsAbstract: true } => defaultValue with { isAbstract = true },
                _ => defaultValue with { hasImplementedMember = true },
            },
            _ => throw new ArgumentOutOfRangeException(nameof(mode), mode, null),
        };

        var comparer = SymbolEqualityComparer.Default;

        return result.isAbstract && comparer.Equals(implicitMemberSymbol!.ContainingType, containingTypeSymbol)
            ? defaultValue with { hasImplementedMember = true }
            : result;
    }

    private static bool IsBaseType(ITypeSymbol symbol)
    {
        return symbol.ToDisplayString(FullyQualifiedFormat) is "object" or "global::System.ValueType";
    }
}
