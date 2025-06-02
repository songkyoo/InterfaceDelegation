using System.Diagnostics;

namespace Macaron.InterfaceDelegation;

[Conditional("SOURCE_GENERATOR_ONLY")]
[AttributeUsage(AttributeTargets.Property | AttributeTargets.Field | AttributeTargets.Parameter)]
public sealed class LiftAttribute(
    string[]? filter = null,
    string[]? remove = null,
    string[]? rename = null
) : Attribute
{
    public string[]? Filter { get; } = filter;

    public string[]? Remove { get; } = remove;

    public string[]? Rename { get; } = rename;
}
