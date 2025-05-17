using Microsoft.CodeAnalysis;

namespace Macaron.InterfaceDelegation;

public class MethodSignatureGenerationHelpers
{
    public static string GetParameterString(IParameterSymbol parameterSymbol)
    {
        var modifiersString = GetParameterModifierString(parameterSymbol);
        var typeString = parameterSymbol.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
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
