using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

namespace Macaron.InterfaceDelegation;

internal abstract record GenerationContext(
    AttributeData Attribute,
    ISymbol DeclaredSymbol,
    ITypeSymbol DelegationTypeSymbol
);

internal sealed record ExposeGenerationContext(
    AttributeData Attribute,
    ISymbol DeclaredSymbol,
    ITypeSymbol DelegationTypeSymbol,
    ImplementationMode Mode
) : GenerationContext(Attribute, DeclaredSymbol, DelegationTypeSymbol);

internal sealed record LiftGenerationContext(
    AttributeData Attribute,
    ISymbol DeclaredSymbol,
    ITypeSymbol DelegationTypeSymbol,
    bool IncludeBaseTypes,
    ImmutableHashSet<string> Filter,
    ImmutableHashSet<string> Remove,
    ImmutableDictionary<string, string> Rename,
    ImmutableArray<ISymbol> PrecomputedTargetMembers
) : GenerationContext(Attribute, DeclaredSymbol, DelegationTypeSymbol);
