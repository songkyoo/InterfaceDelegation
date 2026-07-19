using System.Collections.Immutable;

using static Microsoft.CodeAnalysis.SymbolDisplayFormat;

namespace Macaron.InterfaceDelegation;

internal sealed class ConstrainedFieldDelegationDispatchRenderer(ConstrainedFieldDelegationDispatch dispatch)
    : DelegationDispatchRenderer
{
    private readonly string _interfaceType = dispatch.InterfaceTypeSymbol.ToDisplayString(FullyQualifiedFormat);
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
        builder.Add("{");
        builder.Add($"{Space}{(returnType != "void" ? "return " : "")}__{methodName}(in {_targetName}{(arguments.Length > 0 ? $", {arguments}" : "")});");
        builder.Add("");
        builder.Add($"{Space}#region Local Functions");
        builder.Add($"{Space}static {returnType} __{methodName}<__T>(in __T __impl{(parameters.Length > 0 ? $", {parameters}" : "")}) where __T : {_interfaceType} => __impl.{methodName}{genericParameters}({arguments});");
        builder.Add($"{Space}#endregion");
        builder.Add("}");
    }

    public override void RenderPropertyGetter(
        string propertyType,
        string propertyName,
        ImmutableArray<string>.Builder builder
    )
    {
        builder.Add($"{Space}get");
        builder.Add($"{Space}{{");
        builder.Add($"{Space}{Space}return __Get(in {_targetName});");
        builder.Add("");
        builder.Add($"{Space}{Space}#region Local Functions");
        builder.Add($"{Space}{Space}static {propertyType} __Get<__TImpl>(in __TImpl __impl) where __TImpl : {_interfaceType} => __impl.{propertyName};");
        builder.Add($"{Space}{Space}#endregion");
        builder.Add($"{Space}}}");
    }

    public override void RenderPropertySetter(
        string propertyType,
        string propertyName,
        ImmutableArray<string>.Builder builder
    )
    {
        builder.Add($"{Space}set");
        builder.Add($"{Space}{{");
        builder.Add($"{Space}{Space}__Set(in {_targetName}, value);");
        builder.Add("");
        builder.Add($"{Space}{Space}#region Local Functions");
        builder.Add($"{Space}{Space}static void __Set<__TImpl>(in __TImpl __impl, {propertyType} value) where __TImpl : {_interfaceType} => __impl.{propertyName} = value;");
        builder.Add($"{Space}{Space}#endregion");
        builder.Add($"{Space}}}");
    }

    public override void RenderIndexerGetter(
        string propertyType,
        string parameters,
        string arguments,
        ImmutableArray<string>.Builder builder
    )
    {
        builder.Add($"{Space}get");
        builder.Add($"{Space}{{");
        builder.Add($"{Space}{Space}return __Get(in {_targetName}, {arguments});");
        builder.Add("");
        builder.Add($"{Space}{Space}#region Local Functions");
        builder.Add($"{Space}{Space}static {propertyType} __Get<__TImpl>(in __TImpl __impl, {parameters}) where __TImpl : {_interfaceType} => __impl[{arguments}];");
        builder.Add($"{Space}{Space}#endregion");
        builder.Add($"{Space}}}");
    }

    public override void RenderIndexerSetter(
        string propertyType,
        string parameters,
        string arguments,
        ImmutableArray<string>.Builder builder
    )
    {
        builder.Add($"{Space}set");
        builder.Add($"{Space}{{");
        builder.Add($"{Space}{Space}__Set(in {_targetName}, {arguments}, value);");
        builder.Add("");
        builder.Add($"{Space}{Space}#region Local Functions");
        builder.Add($"{Space}{Space}static void __Set<__TImpl>(in __TImpl __impl, {parameters}, {propertyType} value) where __TImpl : {_interfaceType} => __impl[{arguments}] = value;");
        builder.Add($"{Space}{Space}#endregion");
        builder.Add($"{Space}}}");
    }

    public override void RenderEvent(
        string eventType,
        string eventName,
        ImmutableArray<string>.Builder builder
    )
    {
        builder.Add($"{Space}add");
        builder.Add($"{Space}{{");
        builder.Add($"{Space}{Space}__Add(in {_targetName}, value);");
        builder.Add("");
        builder.Add($"{Space}{Space}#region Local Functions");
        builder.Add($"{Space}{Space}static void __Add<__TImpl>(in __TImpl __impl, {eventType} value) where __TImpl : {_interfaceType} => __impl.{eventName} += value;");
        builder.Add($"{Space}{Space}#endregion");
        builder.Add($"{Space}}}");
        builder.Add($"{Space}remove");
        builder.Add($"{Space}{{");
        builder.Add($"{Space}{Space}__Remove(in {_targetName}, value);");
        builder.Add("");
        builder.Add($"{Space}{Space}#region Local Functions");
        builder.Add($"{Space}{Space}static void __Remove<__TImpl>(in __TImpl __impl, {eventType} value) where __TImpl : {_interfaceType} => __impl.{eventName} -= value;");
        builder.Add($"{Space}{Space}#endregion");
        builder.Add($"{Space}}}");
    }
}
