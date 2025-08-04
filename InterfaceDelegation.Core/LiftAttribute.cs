using System.Diagnostics;

namespace Macaron.InterfaceDelegation;

[Conditional("SOURCE_GENERATOR_ONLY")]
[AttributeUsage(AttributeTargets.Property | AttributeTargets.Field | AttributeTargets.Parameter)]
public sealed class LiftAttribute(
    bool includeBaseTypes = true,
    string[]? filter = null,
    string[]? remove = null,
    string[]? rename = null
) : Attribute
{
    public bool IncludeBaseTypes { get; } = includeBaseTypes;

    public string[]? Filter { get; } = filter;

    public string[]? Remove { get; } = remove;

    public string[]? Rename { get; } = rename;
}
