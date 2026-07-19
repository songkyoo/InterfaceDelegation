using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

using static Microsoft.CodeAnalysis.MethodKind;

namespace Macaron.InterfaceDelegation;

internal static class LiftRenderingPolicy
{
    public static void RenderMember(
        DelegationRenderingCore.RenderContext context,
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
        DelegationRenderingCore.RenderContext context,
        IMethodSymbol methodSymbol,
        ImmutableArray<string>.Builder builder
    )
    {
        if (methodSymbol is not { IsImplicitlyDeclared: false })
        {
            return;
        }

        DelegationRenderingCore.RenderMethod(context, methodSymbol, builder);
    }

    private static void RenderProperty(
        DelegationRenderingCore.RenderContext context,
        IPropertySymbol propertySymbol,
        ImmutableArray<string>.Builder builder
    )
    {
        if (propertySymbol.IsIndexer)
        {
            return;
        }

        DelegationRenderingCore.RenderProperty(context, propertySymbol, builder);
    }

    private static void RenderEvent(
        DelegationRenderingCore.RenderContext context,
        IEventSymbol eventSymbol,
        ImmutableArray<string>.Builder builder
    )
    {
        DelegationRenderingCore.RenderEvent(context, eventSymbol, builder);
    }
}
