using System.Collections.Immutable;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;

namespace Macaron.InterfaceDelegation;

[Generator]
public class InterfaceDelegationGenerator : IIncrementalGenerator
{
    private const string ExposeAttributeMetadataName = "Macaron.InterfaceDelegation.ExposeAttribute";
    private const string LiftAttributeMetadataName = "Macaron.InterfaceDelegation.LiftAttribute";

    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        var exposeTargets = context
            .SyntaxProvider
            .ForAttributeWithMetadataName(
                fullyQualifiedMetadataName: ExposeAttributeMetadataName,
                predicate: static (syntaxNode, _) => DelegationTargetSyntax.IsSupported(syntaxNode),
                transform: static (attributeContext, cancellationToken) => ExposeTargetAnalyzer.Analyze(
                    attributeContext,
                    cancellationToken
                )
            )
            .WithTrackingName("ExposeAnalysisOutput");
        var liftTargets = context
            .SyntaxProvider
            .ForAttributeWithMetadataName(
                fullyQualifiedMetadataName: LiftAttributeMetadataName,
                predicate: static (syntaxNode, _) => DelegationTargetSyntax.IsSupported(syntaxNode),
                transform: static (attributeContext, cancellationToken) => LiftTargetAnalyzer.Analyze(
                    attributeContext,
                    cancellationToken
                )
            )
            .WithTrackingName("LiftAnalysisOutput");

        var exposeSources = exposeTargets
            .SelectMany(static (output, _) => output.Source is { } source
                ? ImmutableArray.Create(source)
                : ImmutableArray<GeneratedSourceOutput>.Empty
            )
            .WithTrackingName("ExposeSourceOutput");
        var liftSources = liftTargets
            .SelectMany(static (output, _) => output.Source is { } source
                ? ImmutableArray.Create(source)
                : ImmutableArray<GeneratedSourceOutput>.Empty
            )
            .WithTrackingName("LiftSourceOutput");

        context.RegisterSourceOutput(exposeSources, static (sourceProductionContext, output) =>
        {
            sourceProductionContext.AddSource(
                hintName: output.HintName,
                sourceText: SourceText.From(output.Source, Encoding.UTF8)
            );
        });
        context.RegisterSourceOutput(liftSources, static (sourceProductionContext, output) =>
        {
            sourceProductionContext.AddSource(
                hintName: output.HintName,
                sourceText: SourceText.From(output.Source, Encoding.UTF8)
            );
        });

        var exposeDiagnostics = exposeTargets
            .SelectMany(static (output, _) => output.Diagnostics)
            .WithTrackingName("ExposeDiagnostics");
        var liftDiagnostics = liftTargets
            .SelectMany(static (output, _) => output.Diagnostics)
            .WithTrackingName("LiftDiagnostics");

        context.RegisterSourceOutput(exposeDiagnostics, static (sourceProductionContext, diagnostic) =>
        {
            sourceProductionContext.ReportDiagnostic(diagnostic);
        });
        context.RegisterSourceOutput(liftDiagnostics, static (sourceProductionContext, diagnostic) =>
        {
            sourceProductionContext.ReportDiagnostic(diagnostic);
        });
    }
}
