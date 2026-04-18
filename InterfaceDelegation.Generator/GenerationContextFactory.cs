using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Macaron.InterfaceDelegation;

internal static class GenerationContextFactory
{
    private const string ExposeAttributeString = "Macaron.InterfaceDelegation.ExposeAttribute";
    private const string LiftAttributeString = "Macaron.InterfaceDelegation.LiftAttribute";

    public static ImmutableArray<(GenerationContext?, ImmutableArray<Diagnostic>)> Create(
        GeneratorSyntaxContext context
    )
    {
        var declaredSymbol = GetDeclaredSymbol(context);
        if (declaredSymbol?.ContainingType.TypeKind is not TypeKind.Class and not TypeKind.Struct)
        {
            return ImmutableArray<(GenerationContext?, ImmutableArray<Diagnostic>)>.Empty;
        }

        var builder = ImmutableArray.CreateBuilder<(GenerationContext?, ImmutableArray<Diagnostic>)>();
        foreach (var attributeData in declaredSymbol.GetAttributes())
        {
            var attributeString = attributeData.AttributeClass?.ToDisplayString();
            if (attributeString == ExposeAttributeString)
            {
                builder.Add(CreateExposeContext(attributeData, declaredSymbol));
            }
            else if (attributeString == LiftAttributeString)
            {
                builder.Add(CreateLiftContext(attributeData, declaredSymbol));
            }
        }

        return builder.ToImmutable();
    }

    private static (GenerationContext?, ImmutableArray<Diagnostic>) CreateExposeContext(
        AttributeData attributeData,
        ISymbol declaredSymbol
    )
    {
        var constructorArguments = attributeData.ConstructorArguments;
        if (declaredSymbol is IPropertySymbol { Type.IsValueType: true })
        {
            return (
                null,
                ImmutableArray.Create(Diagnostic.Create(
                    descriptor: GenerationDiagnostics.ValueTypePropertyCannotBeDelegatedRule,
                    location: declaredSymbol.Locations.FirstOrDefault(),
                    messageArgs: [declaredSymbol.Name]
                ))
            );
        }

        var constructorArgument = constructorArguments[0].Value;
        var interfaceTypeSymbol = constructorArgument == null
            ? GetDeclaredSymbolType(declaredSymbol) as INamedTypeSymbol
            : constructorArgument as INamedTypeSymbol;

        if (interfaceTypeSymbol == null)
        {
            return (
                null,
                ImmutableArray.Create(Diagnostic.Create(
                    descriptor: GenerationDiagnostics.InvalidImplementationTargetRule,
                    location: GetTypeArgumentLocation(attributeData),
                    messageArgs: [constructorArguments[0].Value]
                ))
            );
        }

        if (interfaceTypeSymbol.TypeKind is not TypeKind.Interface || interfaceTypeSymbol.IsUnboundGenericType)
        {
            return (
                null,
                ImmutableArray.Create(Diagnostic.Create(
                    descriptor: GenerationDiagnostics.InvalidImplementationTargetRule,
                    location: GetTypeArgumentLocation(attributeData),
                    messageArgs: [interfaceTypeSymbol.ToDisplayString()]
                ))
            );
        }

        return (
            new GenerationInterfaceContext(
                Attribute: attributeData,
                DeclaredSymbol: declaredSymbol,
                DelegationTypeSymbol: interfaceTypeSymbol,
                Mode: GetImplementationMode(constructorArguments)
            ),
            ImmutableArray<Diagnostic>.Empty
        );
    }

    private static (GenerationContext?, ImmutableArray<Diagnostic>) CreateLiftContext(
        AttributeData attributeData,
        ISymbol declaredSymbol
    )
    {
        var constructorArguments = attributeData.ConstructorArguments;

        return (
            new GenerationLiftContext(
                Attribute: attributeData,
                DeclaredSymbol: declaredSymbol,
                DelegationTypeSymbol: GetDeclaredSymbolType(declaredSymbol),
                IncludeBaseTypes: constructorArguments[0].Value is true,
                Filter: GetStringArray(constructorArguments[1]).ToImmutableHashSet(),
                Remove: GetStringArray(constructorArguments[2]).ToImmutableHashSet(),
                Rename: GetStringArray(constructorArguments[3])
                    .Where(static value => !string.IsNullOrWhiteSpace(value))
                    .Select(ToRenamePair)
                    .Where(static pair => pair != null)
                    .Select(static pair => pair!.Value)
                    .ToImmutableDictionary()
            ),
            ImmutableArray<Diagnostic>.Empty
        );
    }

    private static ISymbol? GetDeclaredSymbol(GeneratorSyntaxContext context)
    {
        return context.Node switch
        {
            FieldDeclarationSyntax { Declaration.Variables: [var decl] } => context.SemanticModel.GetDeclaredSymbol(decl),
            PropertyDeclarationSyntax decl => context.SemanticModel.GetDeclaredSymbol(decl),
            ParameterSyntax decl => context.SemanticModel.GetDeclaredSymbol(decl),
            _ => null,
        };
    }

    public static ITypeSymbol GetDeclaredSymbolType(ISymbol symbol) => symbol switch
    {
        IFieldSymbol fieldSymbol => fieldSymbol.Type,
        IPropertySymbol propertySymbol => propertySymbol.Type,
        IParameterSymbol parameterSymbol => parameterSymbol.Type,
        _ => throw new InvalidOperationException($"Unexpected symbol type: {symbol.GetType().Name}"),
    };

    private static ImplementationMode GetImplementationMode(ImmutableArray<TypedConstant> constructorArguments)
    {
        return (ImplementationMode)(constructorArguments[1].Value ?? 0) switch
        {
            var value and ImplementationMode.Explicit => value,
            _ => ImplementationMode.Implicit,
        };
    }

    private static string[] GetStringArray(TypedConstant constant)
    {
        return !constant.IsNull
            ? constant.Values.Select(static value => (string?)value.Value ?? "").ToArray()
            : [];
    }

    private static KeyValuePair<string, string>? ToRenamePair(string value)
    {
        var values = value.Split(':').Select(static part => part.Trim()).ToArray();
        return values.Length != 2 || values.Any(static part => part.Length < 1)
            ? null
            : new KeyValuePair<string, string>(values[0], values[1]);
    }

    private static Location? GetTypeArgumentLocation(AttributeData attributeData)
    {
        var syntax = attributeData.ApplicationSyntaxReference?.GetSyntax();
        return syntax is AttributeSyntax { ArgumentList: { Arguments.Count: > 0 } argList }
            ? argList.Arguments[0].GetLocation()
            : null;
    }
}
