using Microsoft.CodeAnalysis;
using static Macaron.InterfaceDelegation.DelegationMemberGenerationDecision;
using static Macaron.InterfaceDelegation.DelegationMemberGenerationMode;

namespace Macaron.InterfaceDelegation;

internal static class DelegationMemberGenerationPolicy
{
    public static DelegationMemberGenerationDecision GetDecision(
        DelegationMemberGenerationMode mode,
        ITypeSymbol? targetTypeSymbol,
        ISymbol? implicitMemberSymbol,
        ISymbol? explicitMemberSymbol
    )
    {
        var decision = mode switch
        {
            ImplicitInterfaceImplementation => (implicitMemberSymbol, explicitMemberSymbol) switch
            {
                (null, null) => Generate,
                ({ IsAbstract: true }, null) => OverrideAbstractMember,
                _ => Skip,
            },
            ExplicitInterfaceImplementation => explicitMemberSymbol == null
                ? GenerateExplicitInterfaceImplementation
                : Skip,
            Lift => implicitMemberSymbol switch
            {
                null => Generate,
                { IsAbstract: true } => OverrideAbstractMember,
                _ => Skip,
            },
            _ => throw new ArgumentOutOfRangeException(nameof(mode), mode, null),
        };

        var comparer = SymbolEqualityComparer.Default;

        return decision == OverrideAbstractMember
            && comparer.Equals(implicitMemberSymbol!.ContainingType, targetTypeSymbol)
            ? Skip
            : decision;
    }
}
