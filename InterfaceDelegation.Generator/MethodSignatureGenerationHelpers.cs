using Microsoft.CodeAnalysis;

using static Microsoft.CodeAnalysis.SymbolDisplayFormat;

namespace Macaron.InterfaceDelegation;

public class MethodSignatureGenerationHelpers
{
    public static string GetParameterString(IParameterSymbol parameterSymbol)
    {
        var modifiersString = GetParameterModifierString(parameterSymbol);
        var typeString = parameterSymbol.Type.ToDisplayString(FullyQualifiedFormat);
        var nullabilityString = GetNullableAnnotationString(parameterSymbol, typeString);
        var nameString = parameterSymbol.Name;

        return $"{modifiersString}{typeString}{nullabilityString} {nameString}";

        #region Local Functions
        static string GetNullableAnnotationString(IParameterSymbol parameterSymbol, string typeString) =>
            parameterSymbol.NullableAnnotation == NullableAnnotation.Annotated && !typeString.EndsWith("?")
                ? "?"
                : "";
        #endregion
    }

    public static string GetArgumentString(IParameterSymbol parameterSymbol)
    {
        var prefix = parameterSymbol.RefKind switch
        {
            RefKind.Ref => "ref ",
            RefKind.Out => "out ",
            RefKind.In => "in ",
            _ => ""
        };

        return $"{prefix}{parameterSymbol.Name}";
    }

    public static string GetTypeParameterConstraintClause(ITypeParameterSymbol typeParameter)
    {
        var constraints = new List<string>();

        if (typeParameter.HasReferenceTypeConstraint)
        {
            constraints.Add("class");
        }

        if (typeParameter.HasUnmanagedTypeConstraint)
        {
            constraints.Add("unmanaged");
        }

        if (typeParameter.HasValueTypeConstraint)
        {
            constraints.Add("struct");
        }

        foreach (var constraintType in typeParameter.ConstraintTypes)
        {
            constraints.Add(constraintType.ToDisplayString(FullyQualifiedFormat));
        }

        if (typeParameter.HasConstructorConstraint)
        {
            constraints.Add("new()");
        }

        if (typeParameter.HasNotNullConstraint)
        {
            constraints.Add("not null");
        }

        return constraints.Count == 0 ? "" : $"where {typeParameter.Name} : {string.Join(", ", constraints)}";
    }

    private static string GetParameterModifierString(IParameterSymbol parameterSymbol)
    {
        return parameterSymbol switch
        {
            { RefKind: RefKind.Ref } => "ref ",
            { RefKind: RefKind.Out } => "out ",
            { RefKind: RefKind.In } => "in ",
            { IsParams: true } => "params ",
            _ => "",
        };
    }
}
