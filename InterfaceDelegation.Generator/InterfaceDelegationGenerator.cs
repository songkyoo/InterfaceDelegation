using System.Collections.Immutable;
using System.Runtime.CompilerServices;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

using static Macaron.InterfaceDelegation.SourceGenerationHelper;
using static Microsoft.CodeAnalysis.SymbolDisplayFormat;

namespace Macaron.InterfaceDelegation;

[Generator]
public class InterfaceDelegationGenerator : IIncrementalGenerator
{
    private const string ExposeAttributeMetadataName = "Macaron.InterfaceDelegation.ExposeAttribute";
    private const string LiftAttributeMetadataName = "Macaron.InterfaceDelegation.LiftAttribute";

    private const string Space = "    ";

    private static readonly ConditionalWeakTable<Compilation, ExposeAnalysisCache> ExposeAnalysisCaches = new();

    private readonly record struct ExposeApplication(
        ISymbol DeclaredSymbol,
        AttributeData Attribute,
        int SyntaxTreeIndex,
        int SpanStart
    );

    private readonly record struct ExposeApplicationKey(SyntaxTree SyntaxTree, TextSpan Span);

    private readonly record struct ExposeAnalysisEntry(
        GenerationContext? Context,
        ImmutableArray<Diagnostic> Diagnostics,
        bool IsCanonical
    );

    private sealed class ExposeTypeAnalysis(ImmutableDictionary<ExposeApplicationKey, ExposeAnalysisEntry> entries)
    {
        public bool TryGetEntry(AttributeData attribute, out ExposeAnalysisEntry entry)
        {
            if (attribute.ApplicationSyntaxReference is not { } syntaxReference)
            {
                entry = default;
                return false;
            }

            return entries.TryGetValue(
                new ExposeApplicationKey(syntaxReference.SyntaxTree, syntaxReference.Span),
                out entry
            );
        }
    }

    private sealed class ExposeAnalysisCache
    {
        private readonly object _gate = new();
        private readonly Dictionary<ISymbol, ExposeTypeAnalysis> _typeAnalyses = new(SymbolEqualityComparer.Default);

        public ExposeTypeAnalysis GetOrCreate(
            INamedTypeSymbol typeSymbol,
            Compilation compilation,
            INamedTypeSymbol exposeAttributeSymbol,
            CancellationToken cancellationToken
        )
        {
            lock (_gate)
            {
                if (_typeAnalyses.TryGetValue(typeSymbol, out var analysis))
                {
                    return analysis;
                }
            }

            var created = CreateExposeTypeAnalysis(
                typeSymbol,
                compilation,
                exposeAttributeSymbol,
                cancellationToken
            );

            lock (_gate)
            {
                if (_typeAnalyses.TryGetValue(typeSymbol, out var analysis))
                {
                    return analysis;
                }

                _typeAnalyses.Add(typeSymbol, created);

                return created;
            }
        }
    }

    private static TargetGenerationOutput AnalyzeExposeTarget(
        GeneratorAttributeSyntaxContext attributeContext,
        CancellationToken cancellationToken
    )
    {
        var diagnostics = ImmutableArray.CreateBuilder<Diagnostic>();
        var lines = ImmutableArray.CreateBuilder<string>();
        var exposeAttributeSymbol = attributeContext.Attributes.IsEmpty
            ? null
            : attributeContext.Attributes[0].AttributeClass;

        if (exposeAttributeSymbol == null)
        {
            return new TargetGenerationOutput(null, ImmutableArray<Diagnostic>.Empty);
        }

        var compilation = attributeContext.SemanticModel.Compilation;
        var analysisCache = ExposeAnalysisCaches.GetValue(
            key: compilation,
            createValueCallback: static _ => new ExposeAnalysisCache()
        );
        var typeAnalysis = analysisCache.GetOrCreate(
            typeSymbol: attributeContext.TargetSymbol.ContainingType!,
            compilation,
            exposeAttributeSymbol,
            cancellationToken
        );

        foreach (var attribute in attributeContext.Attributes.OrderBy(GetAttributeSpanStart))
        {
            cancellationToken.ThrowIfCancellationRequested();

            if (!typeAnalysis.TryGetEntry(attribute, out var entry))
            {
                continue;
            }

            diagnostics.AddRange(entry.Diagnostics);

            if (entry.Context is not GenerationInterfaceContext interfaceContext)
            {
                continue;
            }

            if (!entry.IsCanonical)
            {
                diagnostics.Add(Diagnostic.Create(
                    descriptor: GenerationDiagnostics.DuplicateDelegationTargetRule,
                    location: interfaceContext.Attribute.ApplicationSyntaxReference?.GetSyntax(cancellationToken).GetLocation(),
                    messageArgs: [interfaceContext.DelegationTypeSymbol]
                ));

                continue;
            }

            AppendGeneration(lines, interfaceContext);
        }

        return CreateTargetOutput(
            attributeContext.TargetSymbol,
            outputKind: "Expose",
            lines,
            diagnostics.ToImmutable()
        );

        #region Local Functions
        static int GetAttributeSpanStart(AttributeData attributeData)
        {
            return attributeData.ApplicationSyntaxReference?.Span.Start ?? int.MaxValue;
        }
        #endregion
    }

    private static ExposeTypeAnalysis CreateExposeTypeAnalysis(
        INamedTypeSymbol typeSymbol,
        Compilation compilation,
        INamedTypeSymbol exposeAttributeSymbol,
        CancellationToken cancellationToken
    )
    {
        var entries = ImmutableDictionary.CreateBuilder<ExposeApplicationKey, ExposeAnalysisEntry>();
        var delegatedInterfaces = new HashSet<ITypeSymbol>(SymbolEqualityComparer.Default);

        foreach (var application in GetExposeApplications(typeSymbol, exposeAttributeSymbol, cancellationToken))
        {
            cancellationToken.ThrowIfCancellationRequested();

            var (generationContext, diagnostics) = GenerationContextFactory.CreateExposeContext(
                application.Attribute,
                application.DeclaredSymbol,
                compilation,
                cancellationToken
            );
            var isCanonical = generationContext is not GenerationInterfaceContext interfaceContext ||
                delegatedInterfaces.Add(interfaceContext.DelegationTypeSymbol);
            var syntaxReference = application.Attribute.ApplicationSyntaxReference!;

            entries.Add(
                new ExposeApplicationKey(syntaxReference.SyntaxTree, syntaxReference.Span),
                new ExposeAnalysisEntry(generationContext, diagnostics, isCanonical)
            );
        }

        return new ExposeTypeAnalysis(entries.ToImmutable());
    }

    private static TargetGenerationOutput AnalyzeLiftTarget(
        GeneratorAttributeSyntaxContext attributeContext,
        CancellationToken cancellationToken
    )
    {
        var results = GenerationContextFactory.CreateLiftContexts(attributeContext, cancellationToken);
        var diagnostics = ImmutableArray.CreateBuilder<Diagnostic>();
        var lines = ImmutableArray.CreateBuilder<string>();

        foreach (var (generationContext, contextDiagnostics) in results)
        {
            cancellationToken.ThrowIfCancellationRequested();

            diagnostics.AddRange(contextDiagnostics);

            if (generationContext != null)
            {
                AppendGeneration(lines, generationContext);
            }
        }

        return CreateTargetOutput(
            targetSymbol: attributeContext.TargetSymbol,
            outputKind: "Lift",
            lines,
            diagnostics: diagnostics.ToImmutable()
        );
    }

    private static void AppendGeneration(ImmutableArray<string>.Builder builder, GenerationContext generationContext)
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

    private static TargetGenerationOutput CreateTargetOutput(
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

    private static ImmutableArray<ExposeApplication> GetExposeApplications(
        INamedTypeSymbol typeSymbol,
        INamedTypeSymbol exposeAttributeSymbol,
        CancellationToken cancellationToken
    )
    {
        var treeIndexes = new Dictionary<SyntaxTree, int>();
        var syntaxTreeIndex = 0;

        foreach (var syntaxReference in typeSymbol.DeclaringSyntaxReferences)
        {
            cancellationToken.ThrowIfCancellationRequested();

            if (!treeIndexes.ContainsKey(syntaxReference.SyntaxTree))
            {
                treeIndexes[syntaxReference.SyntaxTree] = syntaxTreeIndex++;
            }
        }

        var applications = ImmutableArray.CreateBuilder<ExposeApplication>();

        foreach (var declaredSymbol in GetSupportedTargets(typeSymbol))
        {
            cancellationToken.ThrowIfCancellationRequested();

            foreach (var attribute in declaredSymbol.GetAttributes())
            {
                if (!SymbolEqualityComparer.Default.Equals(attribute.AttributeClass, exposeAttributeSymbol)
                    || attribute.ApplicationSyntaxReference is not { } syntaxReference
                )
                {
                    continue;
                }

                applications.Add(new ExposeApplication(
                    DeclaredSymbol: declaredSymbol,
                    Attribute: attribute,
                    SyntaxTreeIndex: treeIndexes.TryGetValue(syntaxReference.SyntaxTree, out var index)
                        ? index
                        : int.MaxValue,
                    SpanStart: syntaxReference.Span.Start
                ));
            }
        }

        return applications
            .OrderBy(static application => application.SyntaxTreeIndex)
            .ThenBy(static application => application.SpanStart)
            .ToImmutableArray();
    }

    private static IEnumerable<ISymbol> GetSupportedTargets(INamedTypeSymbol typeSymbol)
    {
        var seen = new HashSet<ISymbol>(SymbolEqualityComparer.Default);

        foreach (var memberSymbol in typeSymbol.GetMembers())
        {
            if (GenerationContextFactory.IsSupportedTargetSymbol(memberSymbol) && seen.Add(memberSymbol))
            {
                yield return memberSymbol;
            }
        }

        foreach (var constructorSymbol in typeSymbol.InstanceConstructors)
        {
            foreach (var parameterSymbol in constructorSymbol.Parameters)
            {
                if (GenerationContextFactory.IsSupportedTargetSymbol(parameterSymbol) && seen.Add(parameterSymbol))
                {
                    yield return parameterSymbol;
                }
            }
        }
    }

    private static bool IsSupportedTargetSyntax(SyntaxNode syntaxNode)
    {
        return syntaxNode switch
        {
            FieldDeclarationSyntax { Declaration.Variables.Count: 1 } => true,
            VariableDeclaratorSyntax
            {
                Parent: VariableDeclarationSyntax { Variables.Count: 1 },
            } => true,
            PropertyDeclarationSyntax => true,
            ParameterSyntax
            {
                Parent: ParameterListSyntax
                {
                    Parent: RecordDeclarationSyntax or ClassDeclarationSyntax or StructDeclarationSyntax,
                },
            } => true,
            _ => false,
        };
    }

    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        var exposeTargets = context
            .SyntaxProvider
            .ForAttributeWithMetadataName(
                fullyQualifiedMetadataName: ExposeAttributeMetadataName,
                predicate: static (syntaxNode, _) => IsSupportedTargetSyntax(syntaxNode),
                transform: static (attributeContext, cancellationToken) => AnalyzeExposeTarget(
                    attributeContext,
                    cancellationToken
                )
            )
            .WithTrackingName("ExposeAnalysisOutput");
        var liftTargets = context
            .SyntaxProvider
            .ForAttributeWithMetadataName(
                fullyQualifiedMetadataName: LiftAttributeMetadataName,
                predicate: static (syntaxNode, _) => IsSupportedTargetSyntax(syntaxNode),
                transform: static (attributeContext, cancellationToken) => AnalyzeLiftTarget(
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
