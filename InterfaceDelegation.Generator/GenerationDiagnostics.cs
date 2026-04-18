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

    public static readonly DiagnosticDescriptor LiftMemberNameNotFoundRule = new(
        id: "MAID0004",
        title: "Lift member name was not found",
        messageFormat: "The member '{0}' was not found on '{1}' for Lift option '{2}'",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Warning,
        isEnabledByDefault: true
    );

    public static readonly DiagnosticDescriptor ExposeMemberNotImplementedRule = new(
        id: "MAID0005",
        title: "Expose target does not implement an interface member",
        messageFormat: "The target type '{0}' does not implement interface member '{1}' required by Expose",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true
    );
}
