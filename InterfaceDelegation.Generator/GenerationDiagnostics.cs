using Microsoft.CodeAnalysis;

namespace Macaron.InterfaceDelegation;

internal static class GenerationDiagnostics
{
    public static readonly DiagnosticDescriptor InvalidImplementationTargetRule = new(
        id: "MAID0001",
        title: "Expose attribute requires a non-generic interface type",
        messageFormat: "'{0}' is not a valid type for the Expose attribute. Only non-generic interfaces are allowed.",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true
    );

    public static readonly DiagnosticDescriptor ValueTypePropertyCannotBeDelegatedRule = new(
        id: "MAID0002",
        title: "Value type property cannot be delegated",
        messageFormat: "Property '{0}' is of a value type and cannot be delegated using Expose",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true
    );

    public static readonly DiagnosticDescriptor DuplicateDelegationTargetRule = new(
        id: "MAID0003",
        title: "Duplicate Expose target",
        messageFormat: "The interface '{0}' is delegated more than once in the same type",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true
    );
}
