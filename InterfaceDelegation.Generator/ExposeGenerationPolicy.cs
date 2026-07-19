using Microsoft.CodeAnalysis;

using static Microsoft.CodeAnalysis.SymbolDisplayFormat;

namespace Macaron.InterfaceDelegation;

internal static class ExposeGenerationPolicy
{
    public static bool IsMemberImplementingInterface(GenerationInterfaceContext context)
    {
        var targetTypeSymbol = GenerationContextFactory.GetDeclaredSymbolType(context.DeclaredSymbol);

        return
            !SymbolEqualityComparer.Default.Equals(targetTypeSymbol, context.DelegationTypeSymbol)
            && MemberComparisonHelper.ImplementsInterface(targetTypeSymbol, context.DelegationTypeSymbol);
    }

    public static IEnumerable<ISymbol> GetTargetMembers(GenerationInterfaceContext context)
    {
        foreach (var symbol in DelegationMemberUtilities.GetMembersWithBaseTypes(context.DelegationTypeSymbol))
        {
            if (IsExposableInterfaceMember(symbol))
            {
                yield return symbol;
            }
        }
    }

    public static bool IsExposableInterfaceMember(ISymbol symbol)
    {
        if (symbol.IsStatic || symbol.DeclaredAccessibility != Accessibility.Public)
        {
            return false;
        }

        return symbol switch
        {
            IMethodSymbol { MethodKind: MethodKind.Ordinary } => true,
            IPropertySymbol => true,
            IEventSymbol => true,
            _ => false,
        };
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
            IsAbstract: isAbstract,
            Accessibility: isExplicit ? "" : "public ",
            InterfacePrefix: isExplicit
                ? $"{context.DelegationTypeSymbol.ToDisplayString(FullyQualifiedFormat)}."
                : ""
        );
    }
}
