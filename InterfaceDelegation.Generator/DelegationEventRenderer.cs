using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

using static Microsoft.CodeAnalysis.SymbolDisplayFormat;
using static Microsoft.CodeAnalysis.SymbolDisplayMiscellaneousOptions;

namespace Macaron.InterfaceDelegation;

internal static class DelegationEventRenderer
{
    public static void Render(
        DelegationRenderingContext context,
        IEventSymbol eventSymbol,
        ImmutableArray<string>.Builder builder
    )
    {
        var eventType = eventSymbol.Type.ToDisplayString(FullyQualifiedFormat.WithMiscellaneousOptions(
            IncludeNullableReferenceTypeModifier | UseSpecialTypes
        ));

        AddSpacer(builder);
        builder.Add($"{context.AccessibilityText}{context.OverrideModifier}event {eventType} {context.ExplicitInterfacePrefix}{context.MemberContext.SymbolName}");
        builder.Add("{");
        context.DispatchRenderer.RenderEvent(eventType, eventSymbol.Name, builder);
        builder.Add("}");
    }

    private static void AddSpacer(ImmutableArray<string>.Builder builder)
    {
        if (builder.Count > 0)
        {
            builder.Add("");
        }
    }
}
