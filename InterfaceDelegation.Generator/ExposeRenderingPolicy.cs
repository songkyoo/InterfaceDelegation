using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

using static Microsoft.CodeAnalysis.MethodKind;

namespace Macaron.InterfaceDelegation;

internal static class ExposeRenderingPolicy
{
    public static void RenderMember(
        DelegationRenderingContext context,
        ImmutableArray<string>.Builder builder
    )
    {
        switch (context.MemberContext.Symbol)
        {
            case IMethodSymbol { MethodKind: Ordinary } methodSymbol:
            {
                RenderMethod(context, methodSymbol, builder);

                break;
            }
            case IPropertySymbol propertySymbol:
            {
                RenderProperty(context, propertySymbol, builder);

                break;
            }
            case IEventSymbol eventSymbol:
            {
                RenderEvent(context, eventSymbol, builder);

                break;
            }
        }
    }

    private static void RenderMethod(
        DelegationRenderingContext context,
        IMethodSymbol methodSymbol,
        ImmutableArray<string>.Builder builder
    )
    {
        DelegationMethodRenderer.Render(context, methodSymbol, builder);
    }

    private static void RenderProperty(
        DelegationRenderingContext context,
        IPropertySymbol propertySymbol,
        ImmutableArray<string>.Builder builder
    )
    {
        if (propertySymbol.SetMethod?.IsInitOnly is true)
        {
            return;
        }

        DelegationPropertyRenderer.Render(context, propertySymbol, builder);
    }

    private static void RenderEvent(
        DelegationRenderingContext context,
        IEventSymbol eventSymbol,
        ImmutableArray<string>.Builder builder
    )
    {
        DelegationEventRenderer.Render(context, eventSymbol, builder);
    }
}
