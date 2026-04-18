using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

using static Macaron.InterfaceDelegation.MethodSignatureGenerationHelpers;
using static Microsoft.CodeAnalysis.MethodKind;
using static Microsoft.CodeAnalysis.SymbolDisplayFormat;
using static Microsoft.CodeAnalysis.SymbolDisplayMiscellaneousOptions;

namespace Macaron.InterfaceDelegation;

internal static class DelegationRenderingHelpers
{
    private const string Space = "    ";

    internal readonly record struct RenderContext(
        DelegationMemberHelpers.MemberGenerationContext MemberContext,
        bool IsLiftMode,
        bool IsMemberImplementingInterface,
        bool IsField,
        string DeclaredSymbolName,
        string InterfaceTypeString
    );

    public static bool TryRenderMember(RenderContext context, ImmutableArray<string>.Builder builder)
    {
        return context.MemberContext.Symbol switch
        {
            IMethodSymbol { MethodKind: Ordinary } methodSymbol => TryRenderMethod(context, methodSymbol, builder),
            IPropertySymbol propertySymbol => TryRenderProperty(context, propertySymbol, builder),
            IEventSymbol eventSymbol => TryRenderEvent(context, eventSymbol, builder),
            _ => false,
        };
    }

    private static bool TryRenderMethod(RenderContext context, IMethodSymbol methodSymbol, ImmutableArray<string>.Builder builder)
    {
        if (context.IsLiftMode && methodSymbol is not { IsImplicitlyDeclared: false })
        {
            return false;
        }

        var symbolName = context.MemberContext.SymbolName;
        var isAbstract = context.MemberContext.IsAbstract;
        var accessibility = context.MemberContext.Accessibility;
        var @interface = context.MemberContext.InterfacePrefix;
        var @override = isAbstract ? "override " : "";
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

        if (context.IsMemberImplementingInterface)
        {
            builder.Add("{");

            if (context.IsField)
            {
                builder.Add($"{Space}{(returnType != "void" ? "return " : "")}__{methodName}(in {context.DeclaredSymbolName}{(arguments.Length > 0 ? $", {arguments}" : "")});");
                builder.Add("");
                builder.Add($"{Space}#region Local Functions");
                builder.Add($"{Space}static {returnType} __{methodName}<__T>(in __T __impl{(parameters.Length > 0 ? $", {parameters}" : "")}) where __T : {context.InterfaceTypeString} => __impl.{methodName}{genericParameters}({arguments});");
                builder.Add($"{Space}#endregion");
            }
            else
            {
                builder.Add($"{Space}{(returnType != "void" ? "return " : "")}(({context.InterfaceTypeString}){context.DeclaredSymbolName}).{methodName}({arguments});");
            }

            builder.Add("}");
        }
        else
        {
            builder.Add($"{Space}=> {context.DeclaredSymbolName}.{methodName}{genericParameters}({arguments});");
        }

        return true;
    }

    private static bool TryRenderProperty(RenderContext context, IPropertySymbol propertySymbol, ImmutableArray<string>.Builder builder)
    {
        var isInitOnly = propertySymbol.SetMethod?.IsInitOnly is true;
        if (context.IsLiftMode)
        {
            if (propertySymbol.IsIndexer)
            {
                return false;
            }
        }
        else if (isInitOnly)
        {
            return false;
        }

        var symbolName = context.MemberContext.SymbolName;
        var isAbstract = context.MemberContext.IsAbstract;
        var accessibility = context.MemberContext.Accessibility;
        var @interface = context.MemberContext.InterfacePrefix;
        var @override = isAbstract ? "override " : "";
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
            return true;
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
        return true;
    }

    private static bool TryRenderEvent(RenderContext context, IEventSymbol eventSymbol, ImmutableArray<string>.Builder builder)
    {
        var symbolName = context.MemberContext.SymbolName;
        var isAbstract = context.MemberContext.IsAbstract;
        var accessibility = context.MemberContext.Accessibility;
        var @interface = context.MemberContext.InterfacePrefix;
        var @override = isAbstract ? "override " : "";
        var eventType = eventSymbol.Type.ToDisplayString(FullyQualifiedFormat.WithMiscellaneousOptions(
            IncludeNullableReferenceTypeModifier | UseSpecialTypes
        ));
        var eventName = eventSymbol.Name;

        AddSpacer(builder);
        builder.Add($"{accessibility}{@override}event {eventType} {@interface}{symbolName}");
        builder.Add("{");

        if (context.IsMemberImplementingInterface)
        {
            if (context.IsField)
            {
                builder.Add($"{Space}add");
                builder.Add($"{Space}{{");
                builder.Add($"{Space}{Space}__Add(in {context.DeclaredSymbolName}, value);");
                builder.Add("");
                builder.Add($"{Space}{Space}#region Local Functions");
                builder.Add($"{Space}{Space}static void __Add<__TImpl>(in __TImpl __impl, {eventType} value) where __TImpl : {context.InterfaceTypeString} => __impl.{eventName} += value;");
                builder.Add($"{Space}{Space}#endregion");
                builder.Add($"{Space}}}");
                builder.Add($"{Space}remove");
                builder.Add($"{Space}{{");
                builder.Add($"{Space}{Space}__Remove(in {context.DeclaredSymbolName}, value);");
                builder.Add("");
                builder.Add($"{Space}{Space}#region Local Functions");
                builder.Add($"{Space}{Space}static void __Remove<__TImpl>(in __TImpl __impl, {eventType} value) where __TImpl : {context.InterfaceTypeString} => __impl.{eventName} -= value;");
                builder.Add($"{Space}{Space}#endregion");
                builder.Add($"{Space}}}");
            }
            else
            {
                builder.Add($"{Space}add => (({context.InterfaceTypeString}){context.DeclaredSymbolName}).{eventName} += value;");
                builder.Add($"{Space}remove => (({context.InterfaceTypeString}){context.DeclaredSymbolName}).{eventName} -= value;");
            }
        }
        else
        {
            builder.Add($"{Space}add => {context.DeclaredSymbolName}.{eventName} += value;");
            builder.Add($"{Space}remove => {context.DeclaredSymbolName}.{eventName} -= value;");
        }

        builder.Add("}");
        return true;
    }

    private static void AddIndexerGetter(RenderContext context, ImmutableArray<string>.Builder builder, string propertyType, string parameters, string arguments)
    {
        if (context.IsMemberImplementingInterface)
        {
            if (context.IsField)
            {
                builder.Add($"{Space}get");
                builder.Add($"{Space}{{");
                builder.Add($"{Space}{Space}return __Get(in {context.DeclaredSymbolName}, {arguments});");
                builder.Add("");
                builder.Add($"{Space}{Space}#region Local Functions");
                builder.Add($"{Space}{Space}static {propertyType} __Get<__TImpl>(in __TImpl __impl, {parameters}) where __TImpl : {context.InterfaceTypeString} => __impl[{arguments}];");
                builder.Add($"{Space}{Space}#endregion");
                builder.Add($"{Space}}}");
            }
            else
            {
                builder.Add($"{Space}get => (({context.InterfaceTypeString}){context.DeclaredSymbolName})[{arguments}];");
            }
        }
        else
        {
            builder.Add($"{Space}get => {context.DeclaredSymbolName}[{arguments}];");
        }
    }

    private static void AddIndexerSetter(RenderContext context, ImmutableArray<string>.Builder builder, string propertyType, string parameters, string arguments)
    {
        if (context.IsMemberImplementingInterface)
        {
            if (context.IsField)
            {
                builder.Add($"{Space}set");
                builder.Add($"{Space}{{");
                builder.Add($"{Space}{Space}__Set(in {context.DeclaredSymbolName}, {arguments}, value);");
                builder.Add("");
                builder.Add($"{Space}{Space}#region Local Functions");
                builder.Add($"{Space}{Space}static void __Set<__TImpl>(in __TImpl __impl, {parameters}, {propertyType} value) where __TImpl : {context.InterfaceTypeString} => __impl[{arguments}] = value;");
                builder.Add($"{Space}{Space}#endregion");
                builder.Add($"{Space}}}");
            }
            else
            {
                builder.Add($"{Space}set => (({context.InterfaceTypeString}){context.DeclaredSymbolName})[{arguments}] = value;");
            }
        }
        else
        {
            builder.Add($"{Space}set => {context.DeclaredSymbolName}[{arguments}] = value;");
        }
    }

    private static void AddPropertyGetter(RenderContext context, ImmutableArray<string>.Builder builder, string propertyType, string propertyName)
    {
        if (context.IsMemberImplementingInterface)
        {
            if (context.IsField)
            {
                builder.Add($"{Space}get");
                builder.Add($"{Space}{{");
                builder.Add($"{Space}{Space}return __Get(in {context.DeclaredSymbolName});");
                builder.Add("");
                builder.Add($"{Space}{Space}#region Local Functions");
                builder.Add($"{Space}{Space}static {propertyType} __Get<__TImpl>(in __TImpl __impl) where __TImpl : {context.InterfaceTypeString} => __impl.{propertyName};");
                builder.Add($"{Space}{Space}#endregion");
                builder.Add($"{Space}}}");
            }
            else
            {
                builder.Add($"{Space}get => (({context.InterfaceTypeString}){context.DeclaredSymbolName}).{propertyName};");
            }
        }
        else
        {
            builder.Add($"{Space}get => {context.DeclaredSymbolName}.{propertyName};");
        }
    }

    private static void AddPropertySetter(RenderContext context, ImmutableArray<string>.Builder builder, string propertyType, string propertyName)
    {
        if (context.IsMemberImplementingInterface)
        {
            if (context.IsField)
            {
                builder.Add($"{Space}set");
                builder.Add($"{Space}{{");
                builder.Add($"{Space}{Space}__Set(in {context.DeclaredSymbolName}, value);");
                builder.Add("");
                builder.Add($"{Space}{Space}#region Local Functions");
                builder.Add($"{Space}{Space}static void __Set<__TImpl>(in __TImpl __impl, {propertyType} value) where __TImpl : {context.InterfaceTypeString} => __impl.{propertyName} = value;");
                builder.Add($"{Space}{Space}#endregion");
                builder.Add($"{Space}}}");
            }
            else
            {
                builder.Add($"{Space}set => (({context.InterfaceTypeString}){context.DeclaredSymbolName}).{propertyName} = value;");
            }
        }
        else
        {
            builder.Add($"{Space}set => {context.DeclaredSymbolName}.{propertyName} = value;");
        }
    }

    private static void AddSpacer(ImmutableArray<string>.Builder builder)
    {
        if (builder.Count > 0)
        {
            builder.Add("");
        }
    }
}
