using System.Diagnostics;

using static System.AttributeTargets;

namespace Macaron.InterfaceDelegation;

[Conditional("SOURCE_GENERATOR_ONLY")]
[AttributeUsage(validOn: Property | Field | Parameter)]
public sealed class LiftAttribute(
    string[]? filter = null,
    string[]? remove = null,
    string[]? rename = null
) : Attribute
{
    public bool IncludeBaseTypes { get; set; }

    public string[]? Filter { get; } = filter;

    public string[]? Remove { get; } = remove;

    public string[]? Rename { get; } = rename;
}
