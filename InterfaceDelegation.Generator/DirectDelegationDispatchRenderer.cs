using System.Collections.Immutable;

namespace Macaron.InterfaceDelegation;

internal sealed class DirectDelegationDispatchRenderer(DirectDelegationDispatch dispatch) : DelegationDispatchRenderer
{
    private readonly string _targetName = dispatch.TargetName;

    public override void RenderMethod(
        string returnType,
        string methodName,
        string genericParameters,
        string parameters,
        string arguments,
        ImmutableArray<string>.Builder builder
    )
    {
        builder.Add($"{Space}=> {_targetName}.{methodName}{genericParameters}({arguments});");
    }

    public override void RenderPropertyGetter(
        string propertyType,
        string propertyName,
        ImmutableArray<string>.Builder builder
    )
    {
        builder.Add($"{Space}get => {_targetName}.{propertyName};");
    }

    public override void RenderPropertySetter(
        string propertyType,
        string propertyName,
        ImmutableArray<string>.Builder builder
    )
    {
        builder.Add($"{Space}set => {_targetName}.{propertyName} = value;");
    }

    public override void RenderIndexerGetter(
        string propertyType,
        string parameters,
        string arguments,
        ImmutableArray<string>.Builder builder
    )
    {
        builder.Add($"{Space}get => {_targetName}[{arguments}];");
    }

    public override void RenderIndexerSetter(
        string propertyType,
        string parameters,
        string arguments,
        ImmutableArray<string>.Builder builder
    )
    {
        builder.Add($"{Space}set => {_targetName}[{arguments}] = value;");
    }

    public override void RenderEvent(
        string eventType,
        string eventName,
        ImmutableArray<string>.Builder builder
    )
    {
        builder.Add($"{Space}add => {_targetName}.{eventName} += value;");
        builder.Add($"{Space}remove => {_targetName}.{eventName} -= value;");
    }
}
