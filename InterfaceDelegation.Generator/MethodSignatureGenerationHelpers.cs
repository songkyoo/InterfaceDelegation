using Microsoft.CodeAnalysis;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using static Microsoft.CodeAnalysis.CSharp.SyntaxKind;

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
        var defaultValueString = GetParameterDefaultValueString(parameterSymbol);

        return $"{modifiersString}{typeString}{nullabilityString} {nameString}{defaultValueString}";

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
            constraints.Add(typeParameter.ReferenceTypeConstraintNullableAnnotation == NullableAnnotation.Annotated
                ? "class?"
                : "class"
            );
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

    private static string GetParameterDefaultValueString(IParameterSymbol parameterSymbol)
    {
        if (!parameterSymbol.HasExplicitDefaultValue)
        {
            return "";
        }

        var defaultValue = parameterSymbol.ExplicitDefaultValue;

        if (defaultValue == null)
        {
            var parameterType = parameterSymbol.Type;
            return !parameterType.IsValueType || parameterType.NullableAnnotation == NullableAnnotation.Annotated
                ? " = null"
                : " = default";
        }

        if (parameterSymbol.Type.TypeKind == TypeKind.Enum)
        {
            var enumType = parameterSymbol.Type;
            var fullyQualifiedEnumName = enumType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);

            foreach (var fieldSymbol in enumType.GetMembers().OfType<IFieldSymbol>())
            {
                if (fieldSymbol.HasConstantValue && fieldSymbol.ConstantValue.Equals(defaultValue))
                {
                    return $" = {fullyQualifiedEnumName}.{fieldSymbol.Name}";
                }
            }

            return $" = ({fullyQualifiedEnumName})({defaultValue})";
        }

        var literalExpression = defaultValue switch
        {
            string value => LiteralExpression(StringLiteralExpression, Literal(value)),
            char value => LiteralExpression(CharacterLiteralExpression, Literal(value)),
            bool value => LiteralExpression(value ? TrueLiteralExpression : FalseLiteralExpression),
            byte value => LiteralExpression(NumericLiteralExpression, Literal(value)),
            sbyte value => LiteralExpression(NumericLiteralExpression, Literal(value)),
            short value => LiteralExpression(NumericLiteralExpression, Literal(value)),
            ushort value => LiteralExpression(NumericLiteralExpression, Literal(value)),
            int value => LiteralExpression(NumericLiteralExpression, Literal(value)),
            uint value => LiteralExpression(NumericLiteralExpression, Literal(value)),
            long value => LiteralExpression(NumericLiteralExpression, Literal(value)),
            ulong value => LiteralExpression(NumericLiteralExpression, Literal(value)),
            float value => LiteralExpression(NumericLiteralExpression, Literal(value)),
            double value => LiteralExpression(NumericLiteralExpression, Literal(value)),
            decimal value => LiteralExpression(NumericLiteralExpression, Literal(value)),
            _ => null,
        };

        return $" = {literalExpression?.ToFullString() ?? "default"}";
    }
}
