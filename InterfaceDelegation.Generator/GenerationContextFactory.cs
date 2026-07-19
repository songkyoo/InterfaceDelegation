using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Macaron.InterfaceDelegation;

internal static class GenerationContextFactory
{
    public static ImmutableArray<(GenerationContext?, ImmutableArray<Diagnostic>)> CreateLiftContexts(
        GeneratorAttributeSyntaxContext context,
        CancellationToken cancellationToken
    )
    {
        cancellationToken.ThrowIfCancellationRequested();

        if (!IsSupportedTargetSymbol(context.TargetSymbol))
        {
            return ImmutableArray<(GenerationContext?, ImmutableArray<Diagnostic>)>.Empty;
        }

        var builder = ImmutableArray.CreateBuilder<(GenerationContext?, ImmutableArray<Diagnostic>)>(context.Attributes.Length);

        foreach (var attributeData in context.Attributes.OrderBy(GetAttributeSpanStart))
        {
            cancellationToken.ThrowIfCancellationRequested();

            builder.Add(CreateLiftContext(attributeData, context.TargetSymbol));
        }

        return builder.ToImmutable();
    }

    internal static (GenerationContext?, ImmutableArray<Diagnostic>) CreateExposeContext(
        AttributeData attributeData,
        ISymbol declaredSymbol,
        Compilation compilation,
        CancellationToken cancellationToken = default
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

        var interfaceTypeSymbol = GetExposeInterfaceType(attributeData, declaredSymbol);

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
            interfaceTypeSymbol,
            declaredSymbol.ContainingType,
            compilation,
            cancellationToken
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
        var hasMemberOptions = !filter.IsEmpty || !remove.IsEmpty || !rename.IsEmpty;
        var precomputedTargetMembers = hasMemberOptions
            ? GetLiftConfigurableMembers(delegationTypeSymbol, includeBaseTypes).ToImmutableArray()
            : default;

        return (
            new GenerationLiftContext(
                Attribute: attributeData,
                DeclaredSymbol: declaredSymbol,
                DelegationTypeSymbol: delegationTypeSymbol,
                IncludeBaseTypes: includeBaseTypes,
                Filter: filter,
                Remove: remove,
                Rename: rename,
                PrecomputedTargetMembers: precomputedTargetMembers
            ),
            hasMemberOptions
                ? CreateLiftOptionDiagnostics(attributeData, delegationTypeSymbol, precomputedTargetMembers)
                : ImmutableArray<Diagnostic>.Empty
        );
    }

    private static INamedTypeSymbol? GetExposeInterfaceType(AttributeData attributeData, ISymbol declaredSymbol)
    {
        var constructorArgument = attributeData.ConstructorArguments[0].Value;

        return constructorArgument == null
            ? GetDeclaredSymbolType(declaredSymbol) as INamedTypeSymbol
            : constructorArgument as INamedTypeSymbol;
    }

    internal static bool IsSupportedTargetSymbol(ISymbol symbol)
    {
        if (symbol.ContainingType?.TypeKind is not TypeKind.Class and not TypeKind.Struct)
        {
            return false;
        }

        return symbol switch
        {
            IFieldSymbol fieldSymbol => fieldSymbol.DeclaringSyntaxReferences.Any(static syntaxReference =>
                syntaxReference.GetSyntax() is VariableDeclaratorSyntax
                {
                    Parent: VariableDeclarationSyntax { Variables.Count: 1 },
                }
            ),
            IPropertySymbol propertySymbol => propertySymbol.DeclaringSyntaxReferences.Any(static syntaxReference =>
                syntaxReference.GetSyntax() is PropertyDeclarationSyntax
            ),
            IParameterSymbol parameterSymbol => parameterSymbol.DeclaringSyntaxReferences.Any(static syntaxReference =>
                syntaxReference.GetSyntax() is ParameterSyntax
                {
                    Parent: ParameterListSyntax
                    {
                        Parent: RecordDeclarationSyntax or ClassDeclarationSyntax or StructDeclarationSyntax,
                    },
                }
            ),
            _ => false,
        };
    }

    public static ITypeSymbol GetDeclaredSymbolType(ISymbol symbol) => symbol switch
    {
        IFieldSymbol fieldSymbol => fieldSymbol.Type,
        IPropertySymbol propertySymbol => propertySymbol.Type,
        IParameterSymbol parameterSymbol => parameterSymbol.Type,
        _ => throw new InvalidOperationException($"Unexpected symbol type: {symbol.GetType().Name}"),
    };

    private static int GetAttributeSpanStart(AttributeData attributeData)
    {
        return attributeData.ApplicationSyntaxReference?.Span.Start ?? int.MaxValue;
    }

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
        ImmutableArray<ISymbol> targetMembers
    )
    {
        var availableMemberNames = targetMembers
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
        INamedTypeSymbol interfaceTypeSymbol,
        INamedTypeSymbol containingTypeSymbol,
        Compilation compilation,
        CancellationToken cancellationToken
    )
    {
        if (MemberComparisonHelper.ImplementsInterface(targetTypeSymbol, interfaceTypeSymbol))
        {
            return ImmutableArray<Diagnostic>.Empty;
        }

        var builder = ImmutableArray.CreateBuilder<Diagnostic>();
        var location = GetTypeArgumentLocation(attributeData) ??
            attributeData.ApplicationSyntaxReference?.GetSyntax().GetLocation();
        var hasCompatibleImplementation = MemberComparisonHelper.BuildCompatibleImplementationChecker(
            typeSymbol: targetTypeSymbol,
            interfaceSymbol: interfaceTypeSymbol,
            isAccessible: memberSymbol => compilation.IsSymbolAccessibleWithin(
                symbol: memberSymbol,
                within: containingTypeSymbol,
                throughType: targetTypeSymbol
            )
        );

        foreach (var interfaceMember in DelegationMemberUtilities.GetMembersWithBaseTypes(interfaceTypeSymbol))
        {
            cancellationToken.ThrowIfCancellationRequested();

            if (!ExposeGenerationPolicy.IsExposableInterfaceMember(interfaceMember))
            {
                continue;
            }

            if (!hasCompatibleImplementation(interfaceMember))
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

    private static IEnumerable<ISymbol> GetLiftConfigurableMembers(
        ITypeSymbol delegationTypeSymbol,
        bool includeBaseTypes
    )
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

        return GetStringValuesWithLocations(
            attributeData.ConstructorArguments[parameterIndex],
            argumentSyntax?.Expression
        );
    }

    private static AttributeArgumentSyntax? GetAttributeArgumentSyntax(
        AttributeData attributeData,
        string parameterName
    )
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
