using Microsoft.CodeAnalysis;

using static Microsoft.CodeAnalysis.Accessibility;
using static Microsoft.CodeAnalysis.SymbolDisplayFormat;

namespace Macaron.InterfaceDelegation;

internal readonly record struct DelegationRenderingContext(
    DelegationMemberGenerationContext MemberContext,
    DelegationDispatchRenderer DispatchRenderer
)
{
    public string AccessibilityText => MemberContext.Declaration switch
    {
        ImplicitDelegationMemberDeclaration declaration => GetAccessibilityText(declaration.Accessibility),
        ExplicitInterfaceDelegationMemberDeclaration => "",
        OverrideDelegationMemberDeclaration declaration => GetAccessibilityText(declaration.Accessibility),
        _ => throw new InvalidOperationException($"Invalid member declaration: {MemberContext.Declaration.GetType()}"),
    };

    public string ExplicitInterfacePrefix => MemberContext.Declaration switch
    {
        ExplicitInterfaceDelegationMemberDeclaration declaration =>
            $"{declaration.InterfaceTypeSymbol.ToDisplayString(FullyQualifiedFormat)}.",
        _ => "",
    };

    public string OverrideModifier => MemberContext.Declaration is OverrideDelegationMemberDeclaration
        ? "override "
        : "";

    private static string GetAccessibilityText(Accessibility accessibility)
    {
        return accessibility switch
        {
            Public => "public ",
            Internal => "internal ",
            _ => throw new ArgumentOutOfRangeException(nameof(accessibility), accessibility, null),
        };
    }
}
