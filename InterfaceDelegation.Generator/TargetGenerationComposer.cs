using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

using static Macaron.InterfaceDelegation.SourceGenerationHelper;
using static Microsoft.CodeAnalysis.SymbolDisplayFormat;

namespace Macaron.InterfaceDelegation;

internal static class TargetGenerationComposer
{
    private const string Space = "    ";

    public static void AppendGeneration(
        ImmutableArray<string>.Builder builder,
        GenerationContext generationContext
    )
    {
        var lines = DelegationGenerationPipeline.Generate(generationContext);

        if (lines.IsEmpty)
        {
            return;
        }

        if (builder.Count > 0)
        {
            builder.Add("");
        }

        builder.Add($"#region {generationContext.DelegationTypeSymbol.ToDisplayString(FullyQualifiedFormat)}");
        builder.AddRange(lines);
        builder.Add("#endregion");
    }

    public static TargetGenerationOutput CreateOutput(
        ISymbol targetSymbol,
        string outputKind,
        ImmutableArray<string>.Builder lines,
        ImmutableArray<Diagnostic> diagnostics
    )
    {
        if (lines.Count == 0)
        {
            return new TargetGenerationOutput(null, diagnostics);
        }

        var typeSymbol = targetSymbol.ContainingType;

        return new TargetGenerationOutput(
            new GeneratedSourceOutput(
                HintName: GetHintName(typeSymbol, targetSymbol, outputKind),
                Source: RenderSource(typeSymbol, lines)
            ),
            diagnostics
        );
    }

    private static string RenderSource(INamedTypeSymbol typeSymbol, IEnumerable<string> lines)
    {
        var stringBuilder = CreateStringBuilderWithFileHeader();
        var hasNamespace = !typeSymbol.ContainingNamespace.IsGlobalNamespace;

        if (hasNamespace)
        {
            stringBuilder.AppendLine($"namespace {typeSymbol.ContainingNamespace.ToDisplayString()}");
            stringBuilder.AppendLine("{");
        }

        var nestedTypes = new List<INamedTypeSymbol>();
        var parentType = typeSymbol.ContainingType;

        while (parentType != null)
        {
            nestedTypes.Add(parentType);

            parentType = parentType.ContainingType;
        }

        var depthSpacerText = hasNamespace ? Space : "";

        for (var i = nestedTypes.Count - 1; i >= 0; --i)
        {
            var nestedType = nestedTypes[i];

            stringBuilder.AppendLine($"{depthSpacerText}{GetPartialTypeDeclarationString(nestedType)}");
            stringBuilder.AppendLine($"{depthSpacerText}{{");

            depthSpacerText += Space;
        }

        stringBuilder.AppendLine($"{depthSpacerText}{GetPartialTypeDeclarationString(typeSymbol)}");
        stringBuilder.AppendLine($"{depthSpacerText}{{");

        depthSpacerText += Space;

        foreach (var line in lines)
        {
            stringBuilder.AppendLine($"{(line.Length > 0 ? depthSpacerText : "")}{line}");
        }

        depthSpacerText = depthSpacerText[..^Space.Length];

        stringBuilder.AppendLine($"{depthSpacerText}}}");

        for (var i = 0; i < nestedTypes.Count; ++i)
        {
            depthSpacerText = depthSpacerText[..^Space.Length];

            stringBuilder.AppendLine($"{depthSpacerText}}}");
        }

        if (hasNamespace)
        {
            stringBuilder.AppendLine("}");
        }

        return stringBuilder.ToString();
    }
}
