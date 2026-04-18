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

        var exposeContractDiagnostics = CreateExposeContractDiagnostics(
            attributeData,
            GetDeclaredSymbolType(declaredSymbol),
            interfaceTypeSymbol
        );
        if (!exposeContractDiagnostics.IsEmpty)
        {
            return (null, exposeContractDiagnostics);
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
        var includeBaseTypes = constructorArguments[0].Value is true;
        var filter = GetStringArray(constructorArguments[1]).ToImmutableHashSet();
        var remove = GetStringArray(constructorArguments[2]).ToImmutableHashSet();
        var rename = GetStringArray(constructorArguments[3])
            .Where(static value => !string.IsNullOrWhiteSpace(value))
            .Select(ToRenamePair)
            .Where(static pair => pair != null)
            .Select(static pair => pair!.Value)
            .ToImmutableDictionary();
        var delegationTypeSymbol = GetDeclaredSymbolType(declaredSymbol);

        return (
            new GenerationLiftContext(
                Attribute: attributeData,
                DeclaredSymbol: declaredSymbol,
                DelegationTypeSymbol: delegationTypeSymbol,
                IncludeBaseTypes: includeBaseTypes,
                Filter: filter,
                Remove: remove,
                Rename: rename
            ),
            CreateLiftOptionDiagnostics(attributeData, delegationTypeSymbol, includeBaseTypes)
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

    private static ImmutableArray<Diagnostic> CreateLiftOptionDiagnostics(
        AttributeData attributeData,
        ITypeSymbol delegationTypeSymbol,
        bool includeBaseTypes
    )
    {
        var availableMemberNames = GetLiftConfigurableMembers(delegationTypeSymbol, includeBaseTypes)
            .Select(static symbol => symbol.Name)
            .ToImmutableHashSet();
        var builder = ImmutableArray.CreateBuilder<Diagnostic>();

        AddMissingLiftMemberDiagnostics(
            builder,
            attributeData,
            delegationTypeSymbol,
            availableMemberNames,
            parameterName: "filter"
        );
        AddMissingLiftMemberDiagnostics(
            builder,
            attributeData,
            delegationTypeSymbol,
            availableMemberNames,
            parameterName: "remove"
        );
        AddMissingLiftMemberDiagnostics(
            builder,
            attributeData,
            delegationTypeSymbol,
            availableMemberNames,
            parameterName: "rename",
            getMemberName: static value => ToRenamePair(value)?.Key
        );

        return builder.ToImmutable();
    }

    private static ImmutableArray<Diagnostic> CreateExposeContractDiagnostics(
        AttributeData attributeData,
        ITypeSymbol targetTypeSymbol,
        INamedTypeSymbol interfaceTypeSymbol
    )
    {
        var builder = ImmutableArray.CreateBuilder<Diagnostic>();
        var location = GetTypeArgumentLocation(attributeData) ??
            attributeData.ApplicationSyntaxReference?.GetSyntax().GetLocation();

        foreach (var interfaceMember in DelegationMemberUtilities.GetMembersWithBaseTypes(interfaceTypeSymbol))
        {
            if (interfaceMember is not IMethodSymbol { MethodKind: Microsoft.CodeAnalysis.MethodKind.Ordinary } &&
                interfaceMember is not IPropertySymbol &&
                interfaceMember is not IEventSymbol)
            {
                continue;
            }

            if (!MemberComparisonHelpers.HasCompatibleImplementation(targetTypeSymbol, interfaceTypeSymbol, interfaceMember))
            {
                builder.Add(Diagnostic.Create(
                    descriptor: GenerationDiagnostics.ExposeMemberNotImplementedRule,
                    location: location,
                    messageArgs: [targetTypeSymbol.ToDisplayString(), interfaceMember.ToDisplayString()]
                ));
            }
        }

        return builder.ToImmutable();
    }

    private static IEnumerable<ISymbol> GetLiftConfigurableMembers(ITypeSymbol delegationTypeSymbol, bool includeBaseTypes)
    {
        var members = includeBaseTypes
            ? DelegationMemberUtilities.GetMembersWithBaseTypes(delegationTypeSymbol)
            : DelegationMemberUtilities.GetMembers(delegationTypeSymbol);

        foreach (var symbol in members)
        {
            if (symbol.DeclaredAccessibility is not Accessibility.Public and not Accessibility.Internal)
            {
                continue;
            }

            switch (symbol)
            {
                case IMethodSymbol { MethodKind: MethodKind.Ordinary, IsImplicitlyDeclared: false }:
                case IPropertySymbol { IsIndexer: false }:
                case IEventSymbol:
                    yield return symbol;
                    break;
            }
        }
    }

    private static void AddMissingLiftMemberDiagnostics(
        ImmutableArray<Diagnostic>.Builder builder,
        AttributeData attributeData,
        ITypeSymbol delegationTypeSymbol,
        ImmutableHashSet<string> availableMemberNames,
        string parameterName,
        Func<string, string?>? getMemberName = null
    )
    {
        getMemberName ??= static value => value;

        foreach (var (value, location) in GetAttributeStringValues(attributeData, parameterName))
        {
            var memberName = getMemberName(value);
            if (string.IsNullOrWhiteSpace(memberName) || availableMemberNames.Contains(memberName!))
            {
                continue;
            }

            builder.Add(Diagnostic.Create(
                descriptor: GenerationDiagnostics.LiftMemberNameNotFoundRule,
                location: location,
                messageArgs: [memberName, delegationTypeSymbol.ToDisplayString(), parameterName]
            ));
        }
    }

    private static ImmutableArray<(string Value, Location? Location)> GetAttributeStringValues(
        AttributeData attributeData,
        string parameterName
    )
    {
        var constructor = attributeData.AttributeConstructor;
        if (constructor == null)
        {
            return ImmutableArray<(string Value, Location? Location)>.Empty;
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
            return ImmutableArray<(string Value, Location? Location)>.Empty;
        }

        var argumentSyntax = GetAttributeArgumentSyntax(attributeData, parameterName);
        return GetStringValuesWithLocations(attributeData.ConstructorArguments[parameterIndex], argumentSyntax?.Expression);
    }

    private static AttributeArgumentSyntax? GetAttributeArgumentSyntax(AttributeData attributeData, string parameterName)
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

    private static ImmutableArray<(string Value, Location? Location)> GetStringValuesWithLocations(
        TypedConstant constant,
        ExpressionSyntax? expression
    )
    {
        if (constant.IsNull)
        {
            return ImmutableArray<(string Value, Location? Location)>.Empty;
        }

        var values = constant.Values.Select(static value => (string?)value.Value ?? "").ToImmutableArray();
        if (values.IsEmpty)
        {
            return ImmutableArray<(string Value, Location? Location)>.Empty;
        }

        var expressions = expression switch
        {
            ArrayCreationExpressionSyntax { Initializer.Expressions: var items } => items,
            ImplicitArrayCreationExpressionSyntax { Initializer.Expressions: var items } => items,
            InitializerExpressionSyntax { Expressions: var items } => items,
            { } item => [item],
            _ => default,
        };

        var builder = ImmutableArray.CreateBuilder<(string Value, Location? Location)>(values.Length);
        for (var i = 0; i < values.Length; i++)
        {
            var location = expressions != default && i < expressions.Count
                ? expressions[i].GetLocation()
                : expression?.GetLocation();
            builder.Add((values[i], location));
        }

        return builder.ToImmutable();
    }

    private static Location? GetTypeArgumentLocation(AttributeData attributeData)
    {
        var syntax = attributeData.ApplicationSyntaxReference?.GetSyntax();
        return syntax is AttributeSyntax { ArgumentList: { Arguments.Count: > 0 } argList }
            ? argList.Arguments[0].GetLocation()
            : null;
    }
}
