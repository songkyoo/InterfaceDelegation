using System.Diagnostics;

namespace Macaron.InterfaceDelegation;

[Conditional("SOURCE_GENERATOR_ONLY")]
[AttributeUsage(AttributeTargets.Property | AttributeTargets.Field | AttributeTargets.Parameter, AllowMultiple = true)]
public sealed class ForwardAttribute : Attribute;
