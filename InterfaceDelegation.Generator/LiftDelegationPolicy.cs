using Microsoft.CodeAnalysis;

namespace Macaron.InterfaceDelegation;

internal static class LiftDelegationPolicy
{
    public static IEnumerable<ISymbol> GetTargetMembers(GenerationLiftContext context)
    {
        var symbols = context.IncludeBaseTypes
            ? DelegationMemberHelpers.GetMembersWithBaseTypes(context.DelegationTypeSymbol)
            : DelegationMemberHelpers.GetMembers(context.DelegationTypeSymbol);

        foreach (var symbol in symbols)
        {
            if (!ShouldIncludeSymbol(context, symbol))
            {
                continue;
            }

            yield return symbol;
        }
    }

    public static DelegationMemberHelpers.MemberGenerationContext? CreateMemberGenerationContext(
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
        ) = DelegationMemberHelpers.GetImplementationContext(
            mode: "Lift",
            containingTypeSymbol: typeSymbol,
            implicitMemberSymbol: getImplementedMember(symbol, symbolName, false, false),
            explicitMemberSymbol: getImplementedMember(symbol, symbolName, true, false)
        );

        if (hasImplementedMember)
        {
            return null;
        }

        return new DelegationMemberHelpers.MemberGenerationContext(
            Symbol: symbol,
            SymbolName: symbolName,
            IsExplicit: isExplicit,
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
