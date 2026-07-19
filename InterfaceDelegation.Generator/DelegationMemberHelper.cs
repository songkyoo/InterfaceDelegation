using Microsoft.CodeAnalysis;

using static Microsoft.CodeAnalysis.SymbolDisplayFormat;
using static Microsoft.CodeAnalysis.TypeKind;

namespace Macaron.InterfaceDelegation;

internal static class DelegationMemberHelper
{
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

    public static DelegationMemberGenerationDecision GetGenerationDecision(
        DelegationMemberGenerationMode mode,
        ITypeSymbol? containingTypeSymbol,
        ISymbol? implicitMemberSymbol,
        ISymbol? explicitMemberSymbol
    )
    {
        var decision = mode switch
        {
            DelegationMemberGenerationMode.ImplicitInterfaceImplementation => (
                implicitMemberSymbol,
                explicitMemberSymbol
            ) switch
            {
                (null, null) => DelegationMemberGenerationDecision.Generate,
                ({ IsAbstract: true }, null) => DelegationMemberGenerationDecision.OverrideAbstractMember,
                _ => DelegationMemberGenerationDecision.Skip,
            },
            DelegationMemberGenerationMode.ExplicitInterfaceImplementation => explicitMemberSymbol == null
                ? DelegationMemberGenerationDecision.GenerateExplicitInterfaceImplementation
                : DelegationMemberGenerationDecision.Skip,
            DelegationMemberGenerationMode.Lift => implicitMemberSymbol switch
            {
                null => DelegationMemberGenerationDecision.Generate,
                { IsAbstract: true } => DelegationMemberGenerationDecision.OverrideAbstractMember,
                _ => DelegationMemberGenerationDecision.Skip,
            },
            _ => throw new ArgumentOutOfRangeException(nameof(mode), mode, null),
        };

        var comparer = SymbolEqualityComparer.Default;

        return decision == DelegationMemberGenerationDecision.OverrideAbstractMember
            && comparer.Equals(implicitMemberSymbol!.ContainingType, containingTypeSymbol)
            ? DelegationMemberGenerationDecision.Skip
            : decision;
    }

    private static bool IsBaseType(ITypeSymbol symbol)
    {
        return symbol.ToDisplayString(FullyQualifiedFormat) is "object" or "global::System.ValueType";
    }
}
