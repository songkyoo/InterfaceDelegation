using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

using static Macaron.InterfaceDelegation.MethodSignatureGenerationHelper;
using static Microsoft.CodeAnalysis.Accessibility;
using static Microsoft.CodeAnalysis.SymbolDisplayFormat;
using static Microsoft.CodeAnalysis.SymbolDisplayMiscellaneousOptions;

namespace Macaron.InterfaceDelegation;

internal static class DelegationRenderingCore
{
    private const string Space = "    ";

    internal readonly record struct RenderContext(
        DelegationMemberGenerationContext MemberContext,
        DelegationDispatch Dispatch
    )
    {
        public string AccessibilityText => MemberContext.Declaration switch
        {
            ImplicitDelegationMemberDeclaration declaration => GetAccessibilityText(declaration.Accessibility),
            ExplicitInterfaceDelegationMemberDeclaration => "",
            OverrideDelegationMemberDeclaration declaration => GetAccessibilityText(declaration.Accessibility),
            _ => throw new InvalidOperationException($"Invalid member declaration: {MemberContext.Declaration.GetType()}"),
        };

        public string ExplicitInterfacePrefix => MemberContext.Declaration switch
        {
            ExplicitInterfaceDelegationMemberDeclaration declaration =>
                $"{declaration.InterfaceTypeSymbol.ToDisplayString(FullyQualifiedFormat)}.",
            _ => "",
        };

        public string OverrideModifier => MemberContext.Declaration is OverrideDelegationMemberDeclaration
            ? "override "
            : "";

        public static RenderContext Create(
            DelegationGenerationContext executionContext,
            DelegationMemberGenerationContext memberContext
        )
        {
            return new RenderContext(
                MemberContext: memberContext,
                Dispatch: executionContext.Dispatch
            );
        }
    }

    public static void RenderMethod(
        RenderContext context,
        IMethodSymbol methodSymbol,
        ImmutableArray<string>.Builder builder
    )
    {
        var symbolName = context.MemberContext.SymbolName;
        var accessibility = context.AccessibilityText;
        var @interface = context.ExplicitInterfacePrefix;
        var @override = context.OverrideModifier;
        var genericParameterNames = methodSymbol.TypeParameters.Length > 0
            ? string.Join(", ", methodSymbol.TypeParameters.Select(static symbol => symbol.Name))
            : "";
        var genericParameterConstraints = methodSymbol
            .TypeParameters
            .Select(GetTypeParameterConstraintClause)
            .Where(static constraint => constraint.Length > 0)
            .ToImmutableArray();
        var returnType = methodSymbol.ReturnType.ToDisplayString(FullyQualifiedFormat.WithMiscellaneousOptions(
            IncludeNullableReferenceTypeModifier | UseSpecialTypes
        ));
        var methodName = methodSymbol.Name;
        var genericParameters = genericParameterNames.Length > 0 ? $"<{genericParameterNames}>" : "";
        var parameters = string.Join(", ", methodSymbol.Parameters.Select(GetParameterString));
        var arguments = string.Join(", ", methodSymbol.Parameters.Select(GetArgumentString));

        AddSpacer(builder);
        builder.Add($"{accessibility}{@override}{returnType} {@interface}{symbolName}{genericParameters}({parameters})");

        foreach (var constraint in genericParameterConstraints)
        {
            builder.Add($"{Space}{constraint}");
        }

        switch (context.Dispatch)
        {
            case ConstrainedFieldDelegationDispatch dispatch:
            {
                var interfaceType = dispatch.InterfaceTypeSymbol.ToDisplayString(FullyQualifiedFormat);

                builder.Add("{");
                builder.Add($"{Space}{(returnType != "void" ? "return " : "")}__{methodName}(in {dispatch.TargetName}{(arguments.Length > 0 ? $", {arguments}" : "")});");
                builder.Add("");
                builder.Add($"{Space}#region Local Functions");
                builder.Add($"{Space}static {returnType} __{methodName}<__T>(in __T __impl{(parameters.Length > 0 ? $", {parameters}" : "")}) where __T : {interfaceType} => __impl.{methodName}{genericParameters}({arguments});");
                builder.Add($"{Space}#endregion");
                builder.Add("}");

                break;
            }
            case InterfaceCastDelegationDispatch dispatch:
            {
                var interfaceType = dispatch.InterfaceTypeSymbol.ToDisplayString(FullyQualifiedFormat);

                builder.Add("{");
                builder.Add($"{Space}{(returnType != "void" ? "return " : "")}(({interfaceType}){dispatch.TargetName}).{methodName}({arguments});");
                builder.Add("}");

                break;
            }
            case DirectDelegationDispatch dispatch:
            {
                builder.Add($"{Space}=> {dispatch.TargetName}.{methodName}{genericParameters}({arguments});");

                break;
            }
            default:
                throw new InvalidOperationException($"Invalid dispatch: {context.Dispatch.GetType()}");
        }
    }

    public static void RenderProperty(
        RenderContext context,
        IPropertySymbol propertySymbol,
        ImmutableArray<string>.Builder builder
    )
    {
        var isInitOnly = propertySymbol.SetMethod?.IsInitOnly is true;
        var symbolName = context.MemberContext.SymbolName;
        var accessibility = context.AccessibilityText;
        var @interface = context.ExplicitInterfacePrefix;
        var @override = context.OverrideModifier;
        var propertyType = propertySymbol.Type.ToDisplayString(FullyQualifiedFormat.WithMiscellaneousOptions(
            IncludeNullableReferenceTypeModifier | UseSpecialTypes
        ));
        var propertyName = propertySymbol.Name;

        AddSpacer(builder);

        if (propertySymbol.IsIndexer)
        {
            var parameters = string.Join(", ", propertySymbol.Parameters.Select(GetParameterString));
            var arguments = string.Join(", ", propertySymbol.Parameters.Select(GetArgumentString));

            builder.Add($"{accessibility}{propertyType} {@interface}this[{parameters}]");
            builder.Add("{");

            if (propertySymbol.GetMethod != null)
            {
                AddIndexerGetter(context, builder, propertyType, parameters, arguments);
            }

            if (propertySymbol.SetMethod != null)
            {
                AddIndexerSetter(context, builder, propertyType, parameters, arguments);
            }

            builder.Add("}");
            return;
        }

        builder.Add($"{accessibility}{@override}{propertyType} {@interface}{symbolName}");
        builder.Add("{");

        if (propertySymbol.GetMethod != null)
        {
            AddPropertyGetter(context, builder, propertyType, propertyName);
        }

        if (propertySymbol.SetMethod != null && !isInitOnly)
        {
            AddPropertySetter(context, builder, propertyType, propertyName);
        }

        builder.Add("}");
    }

    public static void RenderEvent(
        RenderContext context,
        IEventSymbol eventSymbol,
        ImmutableArray<string>.Builder builder
    )
    {
        var symbolName = context.MemberContext.SymbolName;
        var accessibility = context.AccessibilityText;
        var @interface = context.ExplicitInterfacePrefix;
        var @override = context.OverrideModifier;
        var eventType = eventSymbol.Type.ToDisplayString(FullyQualifiedFormat.WithMiscellaneousOptions(
            IncludeNullableReferenceTypeModifier | UseSpecialTypes
        ));
        var eventName = eventSymbol.Name;

        AddSpacer(builder);
        builder.Add($"{accessibility}{@override}event {eventType} {@interface}{symbolName}");
        builder.Add("{");

        switch (context.Dispatch)
        {
            case ConstrainedFieldDelegationDispatch dispatch:
                {
                    var interfaceType = dispatch.InterfaceTypeSymbol.ToDisplayString(FullyQualifiedFormat);

                    builder.Add($"{Space}add");
                    builder.Add($"{Space}{{");
                    builder.Add($"{Space}{Space}__Add(in {dispatch.TargetName}, value);");
                    builder.Add("");
                    builder.Add($"{Space}{Space}#region Local Functions");
                    builder.Add($"{Space}{Space}static void __Add<__TImpl>(in __TImpl __impl, {eventType} value) where __TImpl : {interfaceType} => __impl.{eventName} += value;");
                    builder.Add($"{Space}{Space}#endregion");
                    builder.Add($"{Space}}}");
                    builder.Add($"{Space}remove");
                    builder.Add($"{Space}{{");
                    builder.Add($"{Space}{Space}__Remove(in {dispatch.TargetName}, value);");
                    builder.Add("");
                    builder.Add($"{Space}{Space}#region Local Functions");
                    builder.Add($"{Space}{Space}static void __Remove<__TImpl>(in __TImpl __impl, {eventType} value) where __TImpl : {interfaceType} => __impl.{eventName} -= value;");
                    builder.Add($"{Space}{Space}#endregion");
                    builder.Add($"{Space}}}");

                    break;
                }
            case InterfaceCastDelegationDispatch dispatch:
                {
                    var interfaceType = dispatch.InterfaceTypeSymbol.ToDisplayString(FullyQualifiedFormat);

                    builder.Add($"{Space}add => (({interfaceType}){dispatch.TargetName}).{eventName} += value;");
                    builder.Add($"{Space}remove => (({interfaceType}){dispatch.TargetName}).{eventName} -= value;");

                    break;
                }
            case DirectDelegationDispatch dispatch:
                {
                    builder.Add($"{Space}add => {dispatch.TargetName}.{eventName} += value;");
                    builder.Add($"{Space}remove => {dispatch.TargetName}.{eventName} -= value;");

                    break;
                }
            default:
                throw new InvalidOperationException($"Invalid dispatch: {context.Dispatch.GetType()}");
        }

        builder.Add("}");
    }

    private static void AddIndexerGetter(
        RenderContext context,
        ImmutableArray<string>.Builder builder,
        string propertyType,
        string parameters,
        string arguments
    )
    {
        switch (context.Dispatch)
        {
            case ConstrainedFieldDelegationDispatch dispatch:
                {
                    var interfaceType = dispatch.InterfaceTypeSymbol.ToDisplayString(FullyQualifiedFormat);

                    builder.Add($"{Space}get");
                    builder.Add($"{Space}{{");
                    builder.Add($"{Space}{Space}return __Get(in {dispatch.TargetName}, {arguments});");
                    builder.Add("");
                    builder.Add($"{Space}{Space}#region Local Functions");
                    builder.Add($"{Space}{Space}static {propertyType} __Get<__TImpl>(in __TImpl __impl, {parameters}) where __TImpl : {interfaceType} => __impl[{arguments}];");
                    builder.Add($"{Space}{Space}#endregion");
                    builder.Add($"{Space}}}");

                    break;
                }
            case InterfaceCastDelegationDispatch dispatch:
                {
                    var interfaceType = dispatch.InterfaceTypeSymbol.ToDisplayString(FullyQualifiedFormat);

                    builder.Add($"{Space}get => (({interfaceType}){dispatch.TargetName})[{arguments}];");

                    break;
                }
            case DirectDelegationDispatch dispatch:
                {
                    builder.Add($"{Space}get => {dispatch.TargetName}[{arguments}];");

                    break;
                }
            default:
                throw new InvalidOperationException($"Invalid dispatch: {context.Dispatch.GetType()}");
        }
    }

    private static void AddIndexerSetter(
        RenderContext context,
        ImmutableArray<string>.Builder builder,
        string propertyType,
        string parameters,
        string arguments
    )
    {
        switch (context.Dispatch)
        {
            case ConstrainedFieldDelegationDispatch dispatch:
                {
                    var interfaceType = dispatch.InterfaceTypeSymbol.ToDisplayString(FullyQualifiedFormat);

                    builder.Add($"{Space}set");
                    builder.Add($"{Space}{{");
                    builder.Add($"{Space}{Space}__Set(in {dispatch.TargetName}, {arguments}, value);");
                    builder.Add("");
                    builder.Add($"{Space}{Space}#region Local Functions");
                    builder.Add($"{Space}{Space}static void __Set<__TImpl>(in __TImpl __impl, {parameters}, {propertyType} value) where __TImpl : {interfaceType} => __impl[{arguments}] = value;");
                    builder.Add($"{Space}{Space}#endregion");
                    builder.Add($"{Space}}}");

                    break;
                }
            case InterfaceCastDelegationDispatch dispatch:
                {
                    var interfaceType = dispatch.InterfaceTypeSymbol.ToDisplayString(FullyQualifiedFormat);

                    builder.Add($"{Space}set => (({interfaceType}){dispatch.TargetName})[{arguments}] = value;");

                    break;
                }
            case DirectDelegationDispatch dispatch:
                {
                    builder.Add($"{Space}set => {dispatch.TargetName}[{arguments}] = value;");

                    break;
                }
            default:
                throw new InvalidOperationException($"Invalid dispatch: {context.Dispatch.GetType()}");
        }
    }

    private static void AddPropertyGetter(
        RenderContext context,
        ImmutableArray<string>.Builder builder,
        string propertyType,
        string propertyName
    )
    {
        switch (context.Dispatch)
        {
            case ConstrainedFieldDelegationDispatch dispatch:
                {
                    var interfaceType = dispatch.InterfaceTypeSymbol.ToDisplayString(FullyQualifiedFormat);

                    builder.Add($"{Space}get");
                    builder.Add($"{Space}{{");
                    builder.Add($"{Space}{Space}return __Get(in {dispatch.TargetName});");
                    builder.Add("");
                    builder.Add($"{Space}{Space}#region Local Functions");
                    builder.Add($"{Space}{Space}static {propertyType} __Get<__TImpl>(in __TImpl __impl) where __TImpl : {interfaceType} => __impl.{propertyName};");
                    builder.Add($"{Space}{Space}#endregion");
                    builder.Add($"{Space}}}");

                    break;
                }
            case InterfaceCastDelegationDispatch dispatch:
                {
                    var interfaceType = dispatch.InterfaceTypeSymbol.ToDisplayString(FullyQualifiedFormat);

                    builder.Add($"{Space}get => (({interfaceType}){dispatch.TargetName}).{propertyName};");

                    break;
                }
            case DirectDelegationDispatch dispatch:
                {
                    builder.Add($"{Space}get => {dispatch.TargetName}.{propertyName};");

                    break;
                }
            default:
                throw new InvalidOperationException($"Invalid dispatch: {context.Dispatch.GetType()}");
        }
    }

    private static void AddPropertySetter(
        RenderContext context,
        ImmutableArray<string>.Builder builder,
        string propertyType,
        string propertyName
    )
    {
        switch (context.Dispatch)
        {
            case ConstrainedFieldDelegationDispatch dispatch:
            {
                var interfaceType = dispatch.InterfaceTypeSymbol.ToDisplayString(FullyQualifiedFormat);

                builder.Add($"{Space}set");
                builder.Add($"{Space}{{");
                builder.Add($"{Space}{Space}__Set(in {dispatch.TargetName}, value);");
                builder.Add("");
                builder.Add($"{Space}{Space}#region Local Functions");
                builder.Add($"{Space}{Space}static void __Set<__TImpl>(in __TImpl __impl, {propertyType} value) where __TImpl : {interfaceType} => __impl.{propertyName} = value;");
                builder.Add($"{Space}{Space}#endregion");
                builder.Add($"{Space}}}");

                break;
            }
            case InterfaceCastDelegationDispatch dispatch:
            {
                var interfaceType = dispatch.InterfaceTypeSymbol.ToDisplayString(FullyQualifiedFormat);

                builder.Add($"{Space}set => (({interfaceType}){dispatch.TargetName}).{propertyName} = value;");

                break;
            }
            case DirectDelegationDispatch dispatch:
            {
                builder.Add($"{Space}set => {dispatch.TargetName}.{propertyName} = value;");

                break;
            }
            default:
                throw new InvalidOperationException($"Invalid dispatch: {context.Dispatch.GetType()}");
        }
    }

    private static string GetAccessibilityText(Accessibility accessibility)
    {
        return accessibility switch
        {
            Public => "public ",
            Internal => "internal ",
            _ => throw new ArgumentOutOfRangeException(nameof(accessibility), accessibility, null),
        };
    }

    private static void AddSpacer(ImmutableArray<string>.Builder builder)
    {
        if (builder.Count > 0)
        {
            builder.Add("");
        }
    }
}
