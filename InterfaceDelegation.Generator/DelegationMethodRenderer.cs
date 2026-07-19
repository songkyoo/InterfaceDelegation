using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

using static Macaron.InterfaceDelegation.MethodSignatureGenerationHelper;
using static Microsoft.CodeAnalysis.SymbolDisplayFormat;
using static Microsoft.CodeAnalysis.SymbolDisplayMiscellaneousOptions;

namespace Macaron.InterfaceDelegation;

internal static class DelegationMethodRenderer
{
    private const string Space = "    ";

    public static void Render(
        DelegationRenderingContext context,
        IMethodSymbol methodSymbol,
        ImmutableArray<string>.Builder builder
    )
    {
        var symbolName = context.MemberContext.SymbolName;
        var genericParameterNames = methodSymbol.TypeParameters.Length > 0
            ? string.Join(", ", methodSymbol.TypeParameters.Select(static symbol => symbol.Name))
            : "";
        var genericParameterConstraints = methodSymbol
            .TypeParameters
            .Select(GetTypeParameterConstraintClause)
            .Where(static constraint => constraint.Length > 0)
            .ToImmutableArray();
        var returnType = methodSymbol.ReturnType.ToDisplayString(FullyQualifiedFormat.WithMiscellaneousOptions(
            IncludeNullableReferenceTypeModifier | UseSpecialTypes
        ));
        var genericParameters = genericParameterNames.Length > 0 ? $"<{genericParameterNames}>" : "";
        var parameters = string.Join(", ", methodSymbol.Parameters.Select(GetParameterString));
        var arguments = string.Join(", ", methodSymbol.Parameters.Select(GetArgumentString));

        AddSpacer(builder);
        builder.Add($"{context.AccessibilityText}{context.OverrideModifier}{returnType} {context.ExplicitInterfacePrefix}{symbolName}{genericParameters}({parameters})");

        foreach (var constraint in genericParameterConstraints)
        {
            builder.Add($"{Space}{constraint}");
        }

        context.DispatchRenderer.RenderMethod(
            returnType,
            methodName: methodSymbol.Name,
            genericParameters,
            parameters,
            arguments,
            builder
        );
    }

    private static void AddSpacer(ImmutableArray<string>.Builder builder)
    {
        if (builder.Count > 0)
        {
            builder.Add("");
        }
    }
}
