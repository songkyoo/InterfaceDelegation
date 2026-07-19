using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

namespace Macaron.InterfaceDelegation;

internal sealed record TargetGenerationOutput(
    GeneratedSourceOutput? Source,
    ImmutableArray<Diagnostic> Diagnostics
);
