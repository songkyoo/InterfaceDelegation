using System.Collections.Immutable;
using System.Runtime.CompilerServices;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Text;

namespace Macaron.InterfaceDelegation;

internal static class ExposeTargetAnalyzer
{
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

            var created = CreateTypeAnalysis(
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

    private static readonly ConditionalWeakTable<Compilation, ExposeAnalysisCache> AnalysisCaches = new();

    public static TargetGenerationOutput Analyze(
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
        var analysisCache = AnalysisCaches.GetValue(
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

            if (entry.Context is not ExposeGenerationContext exposeContext)
            {
                continue;
            }

            if (!entry.IsCanonical)
            {
                diagnostics.Add(Diagnostic.Create(
                    descriptor: GenerationDiagnostics.DuplicateDelegationTargetRule,
                    location: exposeContext.Attribute.ApplicationSyntaxReference?.GetSyntax(cancellationToken).GetLocation(),
                    messageArgs: [exposeContext.DelegationTypeSymbol]
                ));

                continue;
            }

            TargetGenerationComposer.AppendGeneration(lines, exposeContext);
        }

        return TargetGenerationComposer.CreateOutput(
            attributeContext.TargetSymbol,
            outputKind: "Expose",
            lines,
            diagnostics.ToImmutable()
        );
    }

    private static ExposeTypeAnalysis CreateTypeAnalysis(
        INamedTypeSymbol typeSymbol,
        Compilation compilation,
        INamedTypeSymbol exposeAttributeSymbol,
        CancellationToken cancellationToken
    )
    {
        var entries = ImmutableDictionary.CreateBuilder<ExposeApplicationKey, ExposeAnalysisEntry>();
        var delegatedInterfaces = new HashSet<ITypeSymbol>(SymbolEqualityComparer.Default);

        foreach (var application in GetApplications(typeSymbol, exposeAttributeSymbol, cancellationToken))
        {
            cancellationToken.ThrowIfCancellationRequested();

            var (generationContext, diagnostics) = ExposeContextFactory.Create(
                application.Attribute,
                application.DeclaredSymbol,
                compilation,
                cancellationToken
            );
            var isCanonical = generationContext is not ExposeGenerationContext exposeContext ||
                delegatedInterfaces.Add(exposeContext.DelegationTypeSymbol);
            var syntaxReference = application.Attribute.ApplicationSyntaxReference!;

            entries.Add(
                new ExposeApplicationKey(syntaxReference.SyntaxTree, syntaxReference.Span),
                new ExposeAnalysisEntry(generationContext, diagnostics, isCanonical)
            );
        }

        return new ExposeTypeAnalysis(entries.ToImmutable());
    }

    private static ImmutableArray<ExposeApplication> GetApplications(
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
            if (DelegationTargetSymbol.IsSupported(memberSymbol) && seen.Add(memberSymbol))
            {
                yield return memberSymbol;
            }
        }

        foreach (var constructorSymbol in typeSymbol.InstanceConstructors)
        {
            foreach (var parameterSymbol in constructorSymbol.Parameters)
            {
                if (DelegationTargetSymbol.IsSupported(parameterSymbol) && seen.Add(parameterSymbol))
                {
                    yield return parameterSymbol;
                }
            }
        }
    }

    private static int GetAttributeSpanStart(AttributeData attributeData)
    {
        return attributeData.ApplicationSyntaxReference?.Span.Start ?? int.MaxValue;
    }
}
