using Microsoft.CodeAnalysis;

namespace Macaron.InterfaceDelegation;

internal readonly record struct DelegationMemberGenerationContext(
    ISymbol Symbol,
    string SymbolName,
    bool IsAbstract,
    string Accessibility,
    string InterfacePrefix
);
