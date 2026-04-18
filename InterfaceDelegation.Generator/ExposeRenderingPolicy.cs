using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

using static Microsoft.CodeAnalysis.MethodKind;

namespace Macaron.InterfaceDelegation;

internal static class ExposeRenderingPolicy
{
    public static bool TryRenderMember(
        DelegationRenderingCore.RenderContext context,
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
        DelegationRenderingCore.RenderContext context,
        IMethodSymbol methodSymbol,
        ImmutableArray<string>.Builder builder
    )
    {
        DelegationRenderingCore.RenderMethod(context, methodSymbol, builder);
        return true;
    }

    private static bool TryRenderProperty(
        DelegationRenderingCore.RenderContext context,
        IPropertySymbol propertySymbol,
        ImmutableArray<string>.Builder builder
    )
    {
        if (propertySymbol.SetMethod?.IsInitOnly is true)
        {
            return false;
        }

        DelegationRenderingCore.RenderProperty(context, propertySymbol, builder);
        return true;
    }

    private static bool TryRenderEvent(
        DelegationRenderingCore.RenderContext context,
        IEventSymbol eventSymbol,
        ImmutableArray<string>.Builder builder
    )
    {
        DelegationRenderingCore.RenderEvent(context, eventSymbol, builder);
        return true;
    }
}
