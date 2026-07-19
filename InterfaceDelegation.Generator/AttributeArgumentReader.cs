using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Macaron.InterfaceDelegation;

internal static class AttributeArgumentReader
{
    internal readonly record struct StringArgument(string Value, Location? Location);

    public static ImmutableArray<StringArgument> ReadStringArguments(AttributeData attributeData, string parameterName)
    {
        var constructor = attributeData.AttributeConstructor;

        if (constructor == null)
        {
            return ImmutableArray<StringArgument>.Empty;
        }

        var parameterIndex = -1;

        for (var i = 0; i < constructor.Parameters.Length; i++)
        {
            if (constructor.Parameters[i].Name == parameterName)
            {
                parameterIndex = i;

                break;
            }
        }

        if (parameterIndex < 0 || parameterIndex >= attributeData.ConstructorArguments.Length)
        {
            return ImmutableArray<StringArgument>.Empty;
        }

        var argumentSyntax = FindArgumentSyntax(attributeData, parameterName);

        return PairStringValuesWithLocations(
            constant: attributeData.ConstructorArguments[parameterIndex],
            expression: argumentSyntax?.Expression
        );
    }

    public static Location? GetFirstArgumentLocation(AttributeData attributeData)
    {
        var syntax = attributeData.ApplicationSyntaxReference?.GetSyntax();

        return syntax is AttributeSyntax { ArgumentList: { Arguments.Count: > 0 } argumentList }
            ? argumentList.Arguments[0].GetLocation()
            : null;
    }

    private static AttributeArgumentSyntax? FindArgumentSyntax(AttributeData attributeData, string parameterName)
    {
        var syntax = attributeData.ApplicationSyntaxReference?.GetSyntax() as AttributeSyntax;
        var arguments = syntax?.ArgumentList?.Arguments;
        var constructor = attributeData.AttributeConstructor;

        if (arguments == null || constructor == null)
        {
            return null;
        }

        var parameters = constructor.Parameters;
        var positionalIndex = 0;

        foreach (var argument in arguments.Value)
        {
            if (argument.NameColon is { Name.Identifier.ValueText: var nameColon })
            {
                if (nameColon == parameterName)
                {
                    return argument;
                }

                continue;
            }

            if (argument.NameEquals != null)
            {
                continue;
            }

            if (positionalIndex < parameters.Length && parameters[positionalIndex].Name == parameterName)
            {
                return argument;
            }

            positionalIndex++;
        }

        return null;
    }

    private static ImmutableArray<StringArgument> PairStringValuesWithLocations(
        TypedConstant constant,
        ExpressionSyntax? expression
    )
    {
        if (constant.IsNull)
        {
            return ImmutableArray<StringArgument>.Empty;
        }

        var values = constant.Values.Select(static value => (string?)value.Value ?? "").ToImmutableArray();

        if (values.IsEmpty)
        {
            return ImmutableArray<StringArgument>.Empty;
        }

        var expressions = expression switch
        {
            ArrayCreationExpressionSyntax { Initializer.Expressions: var items } => items,
            ImplicitArrayCreationExpressionSyntax { Initializer.Expressions: var items } => items,
            InitializerExpressionSyntax { Expressions: var items } => items,
            not null => [expression],
            _ => default,
        };

        var builder = ImmutableArray.CreateBuilder<StringArgument>(values.Length);

        for (var i = 0; i < values.Length; i++)
        {
            var location = expressions != default && i < expressions.Count
                ? expressions[i].GetLocation()
                : expression?.GetLocation();

            builder.Add(new StringArgument(values[i], location));
        }

        return builder.ToImmutable();
    }
}
