using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

namespace Macaron.InterfaceDelegation;

internal static class LiftTargetAnalyzer
{
    public static TargetGenerationOutput Analyze(
        GeneratorAttributeSyntaxContext attributeContext,
        CancellationToken cancellationToken
    )
    {
        var results = LiftContextFactory.CreateAll(attributeContext, cancellationToken);
        var diagnostics = ImmutableArray.CreateBuilder<Diagnostic>();
        var lines = ImmutableArray.CreateBuilder<string>();

        foreach (var (generationContext, contextDiagnostics) in results)
        {
            cancellationToken.ThrowIfCancellationRequested();

            diagnostics.AddRange(contextDiagnostics);

            if (generationContext != null)
            {
                TargetGenerationComposer.AppendGeneration(lines, generationContext);
            }
        }

        return TargetGenerationComposer.CreateOutput(
            targetSymbol: attributeContext.TargetSymbol,
            outputKind: GenerationOutputKind.Lift,
            lines,
            diagnostics: diagnostics.ToImmutable()
        );
    }
}
