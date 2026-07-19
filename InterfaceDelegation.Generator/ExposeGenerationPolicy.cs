using Microsoft.CodeAnalysis;

using static Macaron.InterfaceDelegation.DelegationMemberGenerationDecision;
using static Macaron.InterfaceDelegation.DelegationMemberGenerationMode;
using static Macaron.InterfaceDelegation.ImplementationMode;
using static Macaron.InterfaceDelegation.MethodReturnTypeComparison;
using static Microsoft.CodeAnalysis.Accessibility;

namespace Macaron.InterfaceDelegation;

internal static class ExposeGenerationPolicy
{
    public static bool RequiresInterfaceDispatch(ExposeGenerationContext context)
    {
        var targetTypeSymbol = DelegationTargetSymbol.GetDeclaredType(context.DeclaredSymbol);

        return
            !SymbolEqualityComparer.Default.Equals(targetTypeSymbol, context.DelegationTypeSymbol)
            && MemberComparisonHelper.ImplementsInterface(targetTypeSymbol, context.DelegationTypeSymbol);
    }

    public static IEnumerable<ISymbol> GetTargetMembers(ExposeGenerationContext context)
    {
        foreach (var symbol in DelegationMemberProvider.GetMembersIncludingBaseTypes(context.DelegationTypeSymbol))
        {
            if (ExposeMemberRules.IsSupportedInterfaceMember(symbol))
            {
                yield return symbol;
            }
        }
    }

    public static DelegationMemberGenerationContext? CreateMemberGenerationContext(
        ExposeGenerationContext context,
        ISymbol symbol,
        MemberImplementationIndex implementationIndex
    )
    {
        var typeSymbol = context.DeclaredSymbol.ContainingType;
        var symbolName = symbol.Name;
        var mode = symbolName == typeSymbol.Name || context.Mode == Explicit
            ? ExplicitInterfaceImplementation
            : ImplicitInterfaceImplementation;
        var decision = DelegationMemberGenerationPolicy.GetDecision(
            mode,
            targetTypeSymbol: typeSymbol,
            implicitMemberSymbol: implementationIndex.FindImplicit(
                symbol,
                symbolName,
                returnTypeComparison: Match
            ),
            explicitMemberSymbol: implementationIndex.FindExplicit(
                symbol,
                symbolName,
                returnTypeComparison: Match
            )
        );

        if (decision == Skip)
        {
            return null;
        }

        DelegationMemberDeclaration declaration = decision switch
        {
            Generate => new ImplicitDelegationMemberDeclaration(Accessibility: Public),
            GenerateExplicitInterfaceImplementation => new ExplicitInterfaceDelegationMemberDeclaration(
                context.DelegationTypeSymbol
            ),
            OverrideAbstractMember => new OverrideDelegationMemberDeclaration(Accessibility: Public),
            _ => throw new ArgumentOutOfRangeException(nameof(decision), decision, null),
        };

        return new DelegationMemberGenerationContext(
            Symbol: symbol,
            SymbolName: symbolName,
            Declaration: declaration
        );
    }
}
