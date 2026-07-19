using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

namespace Macaron.InterfaceDelegation;

internal readonly record struct GenerationAnalysisResult(
    GenerationContext? Context,
    ImmutableArray<Diagnostic> Diagnostics
);
