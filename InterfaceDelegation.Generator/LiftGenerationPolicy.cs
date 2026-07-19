using Microsoft.CodeAnalysis;

namespace Macaron.InterfaceDelegation;

internal static class LiftGenerationPolicy
{
    public static IEnumerable<ISymbol> GetTargetMembers(LiftGenerationContext context)
    {
        var symbols = !context.PrecomputedTargetMembers.IsDefault
            ? context.PrecomputedTargetMembers
            : context.IncludeBaseTypes
            ? DelegationMemberProvider.GetMembersIncludingBaseTypes(context.DelegationTypeSymbol)
            : DelegationMemberProvider.GetDeclaredMembers(context.DelegationTypeSymbol);

        foreach (var symbol in symbols)
        {
            if (!ShouldIncludeSymbol(context, symbol))
            {
                continue;
            }

            yield return symbol;
        }
    }

    public static DelegationMemberGenerationContext? CreateMemberGenerationContext(
        LiftGenerationContext context,
        ISymbol symbol,
        MemberImplementationIndex implementationIndex
    )
    {
        var typeSymbol = context.DeclaredSymbol.ContainingType;
        var symbolName = context.Rename.TryGetValue(symbol.Name, out var renamed)
            ? renamed
            : symbol.Name;
        var decision = DelegationMemberGenerationPolicy.GetDecision(
            mode: DelegationMemberGenerationMode.Lift,
            targetTypeSymbol: typeSymbol,
            implicitMemberSymbol: implementationIndex.FindImplicit(
                symbol,
                symbolName,
                checkReturnType: false
            ),
            explicitMemberSymbol: implementationIndex.FindExplicit(
                symbol,
                symbolName,
                checkReturnType: false
            )
        );

        if (decision == DelegationMemberGenerationDecision.Skip)
        {
            return null;
        }

        return new DelegationMemberGenerationContext(
            Symbol: symbol,
            SymbolName: symbolName,
            IsAbstract: decision == DelegationMemberGenerationDecision.OverrideAbstractMember,
            Accessibility: $"{symbol.DeclaredAccessibility.ToString().ToLower()} ",
            InterfacePrefix: ""
        );
    }

    private static bool ShouldIncludeSymbol(LiftGenerationContext context, ISymbol symbol)
    {
        if (symbol.DeclaredAccessibility is not Accessibility.Public and not Accessibility.Internal)
        {
            return false;
        }

        if (!context.Filter.IsEmpty && !context.Filter.Contains(symbol.Name))
        {
            return false;
        }

        return !context.Remove.Contains(symbol.Name);
    }
}
