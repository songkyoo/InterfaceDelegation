using Microsoft.CodeAnalysis;

using static Microsoft.CodeAnalysis.SymbolDisplayFormat;

namespace Macaron.InterfaceDelegation;

internal static class ExposeGenerationPolicy
{
    public static bool IsMemberImplementingInterface(GenerationInterfaceContext context)
    {
        return GenerationContextFactory
            .GetDeclaredSymbolType(context.DeclaredSymbol)
            .Interfaces
            .Contains(context.DelegationTypeSymbol, SymbolEqualityComparer.Default);
    }

    public static IEnumerable<ISymbol> GetTargetMembers(GenerationInterfaceContext context)
    {
        foreach (var symbol in DelegationMemberUtilities.GetMembersWithBaseTypes(context.DelegationTypeSymbol))
        {
            yield return symbol;
        }
    }

    public static DelegationMemberUtilities.MemberGenerationContext? CreateMemberGenerationContext(
        GenerationInterfaceContext context,
        ISymbol symbol,
        Func<ISymbol, string, bool, bool, ISymbol?> getImplementedMember
    )
    {
        var typeSymbol = context.DeclaredSymbol.ContainingType;
        var symbolName = symbol.Name;
        var mode = symbolName == typeSymbol.Name || context.Mode == ImplementationMode.Explicit
            ? nameof(ImplementationMode.Explicit)
            : nameof(ImplementationMode.Implicit);
        var (
            hasImplementedMember,
            isExplicit,
            isAbstract
        ) = DelegationMemberUtilities.GetImplementationContext(
            mode: mode,
            containingTypeSymbol: typeSymbol,
            implicitMemberSymbol: getImplementedMember(symbol, symbolName, false, true),
            explicitMemberSymbol: getImplementedMember(symbol, symbolName, true, true)
        );

        if (hasImplementedMember)
        {
            return null;
        }

        return new DelegationMemberUtilities.MemberGenerationContext(
            Symbol: symbol,
            SymbolName: symbolName,
            IsExplicit: isExplicit,
            IsAbstract: isAbstract,
            Accessibility: isExplicit ? "" : "public ",
            InterfacePrefix: isExplicit
                ? $"{context.DelegationTypeSymbol.ToDisplayString(FullyQualifiedFormat)}."
                : ""
        );
    }
}
