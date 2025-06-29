﻿using System.Diagnostics;

namespace Macaron.InterfaceDelegation;

[Conditional("SOURCE_GENERATOR_ONLY")]
[AttributeUsage(AttributeTargets.Property | AttributeTargets.Field | AttributeTargets.Parameter, AllowMultiple = true)]
public sealed class ImplementationOfAttribute(
    Type interfaceType,
    ImplementationMode mode = ImplementationMode.Implicit
) : Attribute
{
    public Type InterfaceType { get; } = interfaceType;

    public ImplementationMode Mode { get; } = mode;
}
