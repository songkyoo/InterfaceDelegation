using System.Collections.Immutable;

using static Microsoft.CodeAnalysis.SymbolDisplayFormat;

namespace Macaron.InterfaceDelegation;

internal sealed class InterfaceCastDelegationDispatchRenderer : DelegationDispatchRenderer
{
    private readonly string _targetExpression;

    public InterfaceCastDelegationDispatchRenderer(InterfaceCastDelegationDispatch dispatch)
    {
        var interfaceType = dispatch.InterfaceTypeSymbol.ToDisplayString(FullyQualifiedFormat);

        _targetExpression = $"(({interfaceType}){dispatch.TargetName})";
    }

    public override void RenderMethod(
        string returnType,
        string methodName,
        string genericParameters,
        string parameters,
        string arguments,
        ImmutableArray<string>.Builder builder
    )
    {
        builder.Add("{");
        builder.Add($"{Space}{(returnType != "void" ? "return " : "")}{_targetExpression}.{methodName}({arguments});");
        builder.Add("}");
    }

    public override void RenderPropertyGetter(
        string propertyType,
        string propertyName,
        ImmutableArray<string>.Builder builder
    )
    {
        builder.Add($"{Space}get => {_targetExpression}.{propertyName};");
    }

    public override void RenderPropertySetter(
        string propertyType,
        string propertyName,
        ImmutableArray<string>.Builder builder
    )
    {
        builder.Add($"{Space}set => {_targetExpression}.{propertyName} = value;");
    }

    public override void RenderIndexerGetter(
        string propertyType,
        string parameters,
        string arguments,
        ImmutableArray<string>.Builder builder
    )
    {
        builder.Add($"{Space}get => {_targetExpression}[{arguments}];");
    }

    public override void RenderIndexerSetter(
        string propertyType,
        string parameters,
        string arguments,
        ImmutableArray<string>.Builder builder
    )
    {
        builder.Add($"{Space}set => {_targetExpression}[{arguments}] = value;");
    }

    public override void RenderEvent(
        string eventType,
        string eventName,
        ImmutableArray<string>.Builder builder
    )
    {
        builder.Add($"{Space}add => {_targetExpression}.{eventName} += value;");
        builder.Add($"{Space}remove => {_targetExpression}.{eventName} -= value;");
    }
}
