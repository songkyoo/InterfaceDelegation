using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

using static Macaron.InterfaceDelegation.MethodSignatureGenerationHelper;
using static Microsoft.CodeAnalysis.SymbolDisplayFormat;
using static Microsoft.CodeAnalysis.SymbolDisplayMiscellaneousOptions;

namespace Macaron.InterfaceDelegation;

internal static class DelegationPropertyRenderer
{
    public static void Render(
        DelegationRenderingContext context,
        IPropertySymbol propertySymbol,
        ImmutableArray<string>.Builder builder
    )
    {
        var propertyType = propertySymbol.Type.ToDisplayString(FullyQualifiedFormat.WithMiscellaneousOptions(
            IncludeNullableReferenceTypeModifier | UseSpecialTypes
        ));

        AddSpacer(builder);

        if (propertySymbol.IsIndexer)
        {
            RenderIndexer(context, propertySymbol, propertyType, builder);

            return;
        }

        builder.Add($"{context.AccessibilityText}{context.OverrideModifier}{propertyType} {context.ExplicitInterfacePrefix}{context.MemberContext.SymbolName}");
        builder.Add("{");

        if (propertySymbol.GetMethod != null)
        {
            context.DispatchRenderer.RenderPropertyGetter(propertyType, propertySymbol.Name, builder);
        }

        if (propertySymbol.SetMethod != null && !propertySymbol.SetMethod.IsInitOnly)
        {
            context.DispatchRenderer.RenderPropertySetter(propertyType, propertySymbol.Name, builder);
        }

        builder.Add("}");
    }

    private static void RenderIndexer(
        DelegationRenderingContext context,
        IPropertySymbol propertySymbol,
        string propertyType,
        ImmutableArray<string>.Builder builder
    )
    {
        var parameters = string.Join(", ", propertySymbol.Parameters.Select(GetParameterString));
        var arguments = string.Join(", ", propertySymbol.Parameters.Select(GetArgumentString));

        builder.Add($"{context.AccessibilityText}{propertyType} {context.ExplicitInterfacePrefix}this[{parameters}]");
        builder.Add("{");

        if (propertySymbol.GetMethod != null)
        {
            context.DispatchRenderer.RenderIndexerGetter(propertyType, parameters, arguments, builder);
        }

        if (propertySymbol.SetMethod != null)
        {
            context.DispatchRenderer.RenderIndexerSetter(propertyType, parameters, arguments, builder);
        }

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
