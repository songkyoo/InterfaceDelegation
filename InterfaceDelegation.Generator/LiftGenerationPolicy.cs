using Microsoft.CodeAnalysis;

namespace Macaron.InterfaceDelegation;

internal static class LiftGenerationPolicy
{
    public static IEnumerable<ISymbol> GetTargetMembers(GenerationLiftContext context)
    {
        var symbols = !context.PrecomputedTargetMembers.IsDefault
            ? context.PrecomputedTargetMembers
            : context.IncludeBaseTypes
                ? DelegationMemberUtilities.GetMembersWithBaseTypes(context.DelegationTypeSymbol)
                : DelegationMemberUtilities.GetMembers(context.DelegationTypeSymbol);

        foreach (var symbol in symbols)
        {
            if (!ShouldIncludeSymbol(context, symbol))
            {
                continue;
            }

            yield return symbol;
        }
    }

    public static DelegationMemberUtilities.MemberGenerationContext? CreateMemberGenerationContext(
        GenerationLiftContext context,
        ISymbol symbol,
        Func<ISymbol, string, bool, bool, ISymbol?> getImplementedMember
    )
    {
        var typeSymbol = context.DeclaredSymbol.ContainingType;
        var symbolName = context.Rename.TryGetValue(symbol.Name, out var renamed)
            ? renamed
            : symbol.Name;
        var (
            hasImplementedMember,
            isExplicit,
            isAbstract
        ) = DelegationMemberUtilities.GetImplementationContext(
            mode: "Lift",
            containingTypeSymbol: typeSymbol,
            implicitMemberSymbol: getImplementedMember(symbol, symbolName, false, false),
            explicitMemberSymbol: getImplementedMember(symbol, symbolName, true, false)
        );

        if (hasImplementedMember)
        {
            return null;
        }

        return new DelegationMemberUtilities.MemberGenerationContext(
            Symbol: symbol,
            SymbolName: symbolName,
            IsAbstract: isAbstract,
            Accessibility: isExplicit ? "" : $"{symbol.DeclaredAccessibility.ToString().ToLower()} ",
            InterfacePrefix: ""
        );
    }

    private static bool ShouldIncludeSymbol(GenerationLiftContext context, ISymbol symbol)
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
