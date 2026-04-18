using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

using static Microsoft.CodeAnalysis.MethodKind;

namespace Macaron.InterfaceDelegation;

internal static class ExposeDelegationRendering
{
    public static bool TryRenderMember(
        DelegationRenderingHelpers.RenderContext context,
        ImmutableArray<string>.Builder builder
    )
    {
        return context.MemberContext.Symbol switch
        {
            IMethodSymbol { MethodKind: Ordinary } methodSymbol => TryRenderMethod(context, methodSymbol, builder),
            IPropertySymbol propertySymbol => TryRenderProperty(context, propertySymbol, builder),
            IEventSymbol eventSymbol => TryRenderEvent(context, eventSymbol, builder),
            _ => false,
        };
    }

    private static bool TryRenderMethod(
        DelegationRenderingHelpers.RenderContext context,
        IMethodSymbol methodSymbol,
        ImmutableArray<string>.Builder builder
    )
    {
        DelegationRenderingHelpers.RenderMethod(context, methodSymbol, builder);
        return true;
    }

    private static bool TryRenderProperty(
        DelegationRenderingHelpers.RenderContext context,
        IPropertySymbol propertySymbol,
        ImmutableArray<string>.Builder builder
    )
    {
        if (propertySymbol.SetMethod?.IsInitOnly is true)
        {
            return false;
        }

        DelegationRenderingHelpers.RenderProperty(context, propertySymbol, builder);
        return true;
    }

    private static bool TryRenderEvent(
        DelegationRenderingHelpers.RenderContext context,
        IEventSymbol eventSymbol,
        ImmutableArray<string>.Builder builder
    )
    {
        DelegationRenderingHelpers.RenderEvent(context, eventSymbol, builder);
        return true;
    }
}
