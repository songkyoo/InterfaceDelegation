using System.Collections.Immutable;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

using static Macaron.InterfaceDelegation.MethodSignatureGenerationHelpers;
using static Macaron.InterfaceDelegation.MemberComparisonHelpers;
using static Macaron.InterfaceDelegation.SourceGenerationHelpers;
using static Microsoft.CodeAnalysis.Accessibility;
using static Microsoft.CodeAnalysis.MethodKind;
using static Microsoft.CodeAnalysis.SymbolDisplayFormat;
using static Microsoft.CodeAnalysis.SymbolDisplayMiscellaneousOptions;

namespace Macaron.InterfaceDelegation;

[Generator]
public class InterfaceDelegationGenerator : IIncrementalGenerator
{
    #region Constants
    private const string Implicit = nameof(ImplementationMode.Implicit);
    private const string Explicit = nameof(ImplementationMode.Explicit);
    private const string Lift = nameof(Lift);

    private const string ImplementationOfAttributeString = "Macaron.InterfaceDelegation.ImplementationOfAttribute";
    private const string LiftAttributeString = "Macaron.InterfaceDelegation.LiftAttribute";

    private const string Space = "    ";
    #endregion

    #region Types
    private abstract record GenerationContext(
        AttributeData Attribute,
        ISymbol DeclaredSymbol,
        ITypeSymbol DelegationTypeSymbol
    );

    private sealed record GenerationInterfaceContext(
        AttributeData Attribute,
        ISymbol DeclaredSymbol,
        ITypeSymbol DelegationTypeSymbol,
        ImplementationMode Mode
    ) : GenerationContext(Attribute, DeclaredSymbol, DelegationTypeSymbol);

    private sealed record GenerationLiftContext(
        AttributeData Attribute,
        ISymbol DeclaredSymbol,
        ITypeSymbol DelegationTypeSymbol,
        bool IncludeBaseTypes,
        ImmutableHashSet<string> Filter,
        ImmutableHashSet<string> Remove,
        ImmutableDictionary<string, string> Rename
    ) : GenerationContext(Attribute, DeclaredSymbol, DelegationTypeSymbol);
    #endregion

    #region Diagnostic Descriptors
    private static readonly DiagnosticDescriptor InvalidImplementationTargetRule = new(
        id: "MAID0001",
        title: "ImplementationOf attribute requires a non-generic interface type",
        messageFormat: "'{0}' is not a valid type for the ImplementationOf attribute. Only non-generic interfaces are allowed.",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true
    );
    private static readonly DiagnosticDescriptor ValueTypePropertyCannotBeDelegatedRule = new(
        id: "MAID0002",
        title: "Value type property cannot be delegated",
        messageFormat: "Property '{0}' is of a value type and cannot be delegated using ImplementationOf",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true
    );
    private static readonly DiagnosticDescriptor DuplicateDelegationTargetRule = new(
        id: "MAID0003",
        title: "Duplicate ImplementationOf target",
        messageFormat: "The interface '{0}' is delegated more than once in the same type",
        category: "Usage",
        defaultSeverity: DiagnosticSeverity.Error,
        isEnabledByDefault: true
    );
    #endregion

    #region Static
    private static ImmutableArray<(GenerationContext?, ImmutableArray<Diagnostic>)> GetGenerationContexts(
        GeneratorSyntaxContext context
    )
    {
        var declaredSymbol = context.Node switch
        {
            FieldDeclarationSyntax { Declaration.Variables: [var decl] } => GetDeclaredSymbol(decl),
            PropertyDeclarationSyntax decl => GetDeclaredSymbol(decl),
            ParameterSyntax decl => GetDeclaredSymbol(decl),
            _ => null,
        };
        if (declaredSymbol?.ContainingType.TypeKind is not TypeKind.Class and not TypeKind.Struct)
        {
            return ImmutableArray<(GenerationContext?, ImmutableArray<Diagnostic>)>.Empty;
        }

        var builder = ImmutableArray.CreateBuilder<(GenerationContext?, ImmutableArray<Diagnostic>)>();
        foreach (var attributeData in declaredSymbol.GetAttributes())
        {
            var constructorArguments = attributeData.ConstructorArguments;
            var attributeString = attributeData.AttributeClass?.ToDisplayString();

            if (attributeString == ImplementationOfAttributeString)
            {
                if (declaredSymbol is IPropertySymbol { Type.IsValueType: true })
                {
                    builder.Add((
                        (GenerationInterfaceContext?)null,
                        ImmutableArray.Create(Diagnostic.Create(
                            descriptor: ValueTypePropertyCannotBeDelegatedRule,
                            location: declaredSymbol.Locations.FirstOrDefault(),
                            messageArgs: [declaredSymbol.Name]
                        ))
                    ));
                }
                else if (constructorArguments[0].Value is INamedTypeSymbol interfaceTypeSymbol)
                {
                    if (interfaceTypeSymbol.TypeKind is not TypeKind.Interface || interfaceTypeSymbol.IsUnboundGenericType)
                    {
                        builder.Add((
                            (GenerationInterfaceContext?)null,
                            ImmutableArray.Create(Diagnostic.Create(
                                descriptor: InvalidImplementationTargetRule,
                                location: GetTypeArgumentLocation(attributeData),
                                messageArgs: [interfaceTypeSymbol.ToDisplayString()]
                            ))
                        ));
                        continue;
                    }

                    builder.Add((
                        new GenerationInterfaceContext(
                            Attribute: attributeData,
                            DeclaredSymbol: declaredSymbol,
                            DelegationTypeSymbol: interfaceTypeSymbol,
                            Mode: GetImplementationMode(constructorArguments)
                        ),
                        ImmutableArray<Diagnostic>.Empty
                    ));
                }
                else
                {
                    builder.Add((
                        (GenerationContext?)null,
                        ImmutableArray.Create(Diagnostic.Create(
                            descriptor: InvalidImplementationTargetRule,
                            location: GetTypeArgumentLocation(attributeData),
                            messageArgs: [constructorArguments[0].Value]
                        ))
                    ));
                }
            }
            else if (attributeString == LiftAttributeString)
            {
                builder.Add((
                    new GenerationLiftContext(
                        Attribute: attributeData,
                        DeclaredSymbol: declaredSymbol,
                        DelegationTypeSymbol: GetDeclaredSymbolType(declaredSymbol),
                        IncludeBaseTypes: constructorArguments[0].Value is true,
                        Filter: GetStringArray(constructorArguments[1]).ToImmutableHashSet(),
                        Remove: GetStringArray(constructorArguments[2]).ToImmutableHashSet(),
                        Rename: GetStringArray(constructorArguments[3])
                            .Where(IsNotNullOrWhiteSpace)
                            .Select(ToPair)
                            .Where(pair => pair != null)
                            .Select(pair => pair!.Value)
                            .ToImmutableDictionary()
                    ),
                    ImmutableArray<Diagnostic>.Empty
                ));

                #region Local Functions
                static string[] GetStringArray(TypedConstant constant)
                {
                    return !constant.IsNull
                        ? constant.Values.Select(static constant => (string?)constant.Value ?? "").ToArray()
                        : [];
                }

                static bool IsNotNullOrWhiteSpace(string? value)
                {
                    return !string.IsNullOrWhiteSpace(value);
                }

                static KeyValuePair<string, string>? ToPair(string value)
                {
                    var values = value.Split(':').Select(static value => value.Trim()).ToArray();
                    return values.Length != 2 || values.Any(static value => value.Length < 1)
                        ? null
                        : new KeyValuePair<string, string>(values[0], values[1]);
                }
                #endregion
            }

            #region Local Functions
            static Location? GetTypeArgumentLocation(AttributeData attributeData)
            {
                var syntax = attributeData.ApplicationSyntaxReference?.GetSyntax();
                return syntax is AttributeSyntax { ArgumentList: { Arguments.Count: > 0 } argList }
                    ? argList.Arguments[0].GetLocation()
                    : null;
            }
            #endregion
        }

        return builder.ToImmutable();

        #region Local Functions
        static ImplementationMode GetImplementationMode(ImmutableArray<TypedConstant> constructorArguments)
        {
            return (ImplementationMode)(constructorArguments[1].Value ?? 0) switch
            {
                var value and ImplementationMode.Explicit => value,
                _ => ImplementationMode.Implicit,
            };
        }

        ISymbol? GetDeclaredSymbol(SyntaxNode node) => context.SemanticModel.GetDeclaredSymbol(node);
        #endregion
    }

    private static ImmutableArray<string> GenerateDelegationCode(GenerationContext context)
    {
        var (
            _,
            declaredSymbol,
            delegationTypeSymbol
        ) = context;

        var isLiftMode = context is GenerationLiftContext;
        var isMemberImplementingInterface =
            !isLiftMode && IsImplementingInterface(GetDeclaredSymbolType(declaredSymbol), delegationTypeSymbol);
        var isField = declaredSymbol is IFieldSymbol;

        var typeSymbol = declaredSymbol.ContainingType;

        var typeName = typeSymbol.Name;
        var declaredSymbolName = declaredSymbol.Name;
        var interfaceTypeString = isLiftMode ? "" : delegationTypeSymbol.ToDisplayString(FullyQualifiedFormat);

        var getImplementedMember = BuildMemberComparer(typeSymbol, delegationTypeSymbol);
        var builder = ImmutableArray.CreateBuilder<string>();
        var includeBaseTypes = !isLiftMode || ((GenerationLiftContext)context).IncludeBaseTypes;

        foreach (var symbol in includeBaseTypes
            ? GetMembersWithBaseTypes(delegationTypeSymbol)
            : delegationTypeSymbol.GetMembers()
        )
        {
            var symbolName = symbol.Name;

            if (isLiftMode)
            {
                if (symbol.DeclaredAccessibility is not Public and not Internal ||
                    symbol.IsAbstract
                )
                {
                    continue;
                }

                var liftContext = (GenerationLiftContext)context;
                var filter = liftContext.Filter;
                var remove = liftContext.Remove;

                if (!filter.IsEmpty && !filter.Contains(symbolName))
                {
                    continue;
                }

                if (remove.Contains(symbolName))
                {
                    continue;
                }
            }

            symbolName = (context as GenerationLiftContext)?.Rename.TryGetValue(symbolName, out var newName) is true
                ? newName
                : symbolName;

            var checkReturnType = !isLiftMode;
            var (
                hasImplementedMember,
                isExplicit,
                isAbstract
            ) = GetImplementationContext(
                mode:
                    isLiftMode ? Lift :
                    symbolName == typeName ? Explicit :
                    ((GenerationInterfaceContext)context).Mode == ImplementationMode.Explicit ? Explicit : Implicit,
                containingTypeSymbol: typeSymbol,
                implicitMemberSymbol: getImplementedMember(symbol, symbolName, /* isExplicit */false, checkReturnType),
                explicitMemberSymbol: getImplementedMember(symbol, symbolName, /* isExplicit */true, checkReturnType)
            );

            if (hasImplementedMember)
            {
                continue;
            }

            var accessibility =
                isExplicit ? "" :
                isLiftMode ? $"{symbol.DeclaredAccessibility.ToString().ToLower()} " : "public ";
            var @override = isAbstract ? "override " : "";
            var @interface = isExplicit
                ? $"{delegationTypeSymbol.ToDisplayString(FullyQualifiedFormat)}."
                : "";

            if (symbol is IMethodSymbol { MethodKind: Ordinary } methodSymbol)
            {
                if (isLiftMode)
                {
                    if (methodSymbol is not { IsImplicitlyDeclared: false })
                    {
                        continue;
                    }
                }

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

                if (builder.Count > 0)
                {
                    builder.Add("");
                }

                builder.Add($"{accessibility}{@override}{returnType} {@interface}{symbolName}{genericParameters}({parameters})");

                foreach (var constraint in genericParameterConstraints)
                {
                    builder.Add($"{Space}{constraint}");
                }

                if (isMemberImplementingInterface)
                {
                    builder.Add($"{{");

                    if (isField)
                    {
                        builder.Add($"{Space}{(returnType != "void" ? "return " : "")}__{methodName}(ref {declaredSymbolName}{(arguments.Length > 0 ? $", {arguments}" : "")});");
                        builder.Add($"");
                        builder.Add($"{Space}#region Local Functions");
                        builder.Add($"{Space}static {returnType} __{methodName}<__T>(ref __T __impl{(parameters.Length > 0 ? $", {parameters}" : "")}) where __T : {interfaceTypeString} => __impl.{methodName}{genericParameters}({arguments});");
                        builder.Add($"{Space}#endregion");
                    }
                    else
                    {
                        builder.Add($"{Space}{(returnType != "void" ? "return " : "")}(({interfaceTypeString}){declaredSymbolName}).{methodName}({(arguments.Length > 0 ? $"{arguments}" : "")});");
                    }

                    builder.Add($"}}");
                }
                else
                {
                    builder.Add($"{Space}=> {declaredSymbolName}.{methodName}{genericParameters}({arguments});");
                }
            }
            else if (symbol is IPropertySymbol propertySymbol)
            {
                var isInitOnly = propertySymbol.SetMethod?.IsInitOnly is true;

                if (isLiftMode)
                {
                    if (propertySymbol.IsIndexer)
                    {
                        continue;
                    }
                }
                else
                {
                    if (isInitOnly)
                    {
                        continue;
                    }
                }

                if (builder.Count > 0)
                {
                    builder.Add("");
                }

                var propertyType = propertySymbol.Type.ToDisplayString(FullyQualifiedFormat.WithMiscellaneousOptions(
                    IncludeNullableReferenceTypeModifier | UseSpecialTypes
                ));
                var propertyName = propertySymbol.Name;

                if (propertySymbol.IsIndexer)
                {
                    var parameters = string.Join(", ", propertySymbol.Parameters.Select(GetParameterString));
                    var arguments = string.Join(", ", propertySymbol.Parameters.Select(GetArgumentString));

                    builder.Add($"{accessibility}{propertyType} {@interface}this[{parameters}]");
                    builder.Add($"{{");

                    if (propertySymbol.GetMethod != null)
                    {
                        if (isMemberImplementingInterface)
                        {
                            if (isField)
                            {
                                builder.Add($"{Space}get");
                                builder.Add($"{Space}{{");
                                builder.Add($"{Space}{Space}return __Get(ref {declaredSymbolName}, {arguments});");
                                builder.Add($"");
                                builder.Add($"{Space}{Space}#region Local Functions");
                                builder.Add($"{Space}{Space}static {propertyType} __Get<__TImpl>(ref __TImpl __impl, {parameters}) where __TImpl : {interfaceTypeString} => __impl[{arguments}];");
                                builder.Add($"{Space}{Space}#endregion");
                                builder.Add($"{Space}}}");
                            }
                            else
                            {
                                builder.Add($"{Space}get => (({interfaceTypeString}){declaredSymbolName})[{arguments}];");
                            }
                        }
                        else
                        {
                            builder.Add($"{Space}get => {declaredSymbolName}[{arguments}];");
                        }
                    }

                    if (propertySymbol.SetMethod != null)
                    {
                        if (isMemberImplementingInterface)
                        {
                            if (isField)
                            {
                                builder.Add($"{Space}set");
                                builder.Add($"{Space}{{");
                                builder.Add($"{Space}{Space}__Set(ref {declaredSymbolName}, {arguments}, value);");
                                builder.Add($"");
                                builder.Add($"{Space}{Space}#region Local Functions");
                                builder.Add($"{Space}{Space}static void __Set<__TImpl>(ref __TImpl __impl, {parameters}, {propertyType} value) where __TImpl : {interfaceTypeString} => __impl[{arguments}] = value;");
                                builder.Add($"{Space}{Space}#endregion");
                                builder.Add($"{Space}}}");
                            }
                            else
                            {
                                builder.Add($"{Space}set => (({interfaceTypeString}){declaredSymbolName})[{arguments}] = value;");
                            }
                        }
                        else
                        {
                            builder.Add($"{Space}set => {declaredSymbolName}[{arguments}] = value;");
                        }
                    }

                    builder.Add($"}}");
                }
                else
                {
                    builder.Add($"{accessibility}{@override}{propertyType} {@interface}{symbolName}");
                    builder.Add($"{{");

                    if (propertySymbol.GetMethod != null)
                    {
                        if (isMemberImplementingInterface)
                        {
                            if (isField)
                            {
                                builder.Add($"{Space}get");
                                builder.Add($"{Space}{{");
                                builder.Add($"{Space}{Space}return __Get(ref {declaredSymbolName});");
                                builder.Add($"");
                                builder.Add($"{Space}{Space}#region Local Functions");
                                builder.Add($"{Space}{Space}static {propertyType} __Get<__TImpl>(ref __TImpl __impl) where __TImpl : {interfaceTypeString} => __impl.{propertyName};");
                                builder.Add($"{Space}{Space}#endregion");
                                builder.Add($"{Space}}}");
                            }
                            else
                            {
                                builder.Add($"{Space}get => (({interfaceTypeString}){declaredSymbolName}).{propertyName};");
                            }
                        }
                        else
                        {
                            builder.Add($"{Space}get => {declaredSymbolName}.{propertyName};");
                        }
                    }

                    if (propertySymbol.SetMethod != null && !isInitOnly)
                    {
                        if (isMemberImplementingInterface)
                        {
                            if (isField)
                            {
                                builder.Add($"{Space}set");
                                builder.Add($"{Space}{{");
                                builder.Add($"{Space}{Space}__Set(ref {declaredSymbolName}, value);");
                                builder.Add($"");
                                builder.Add($"{Space}{Space}#region Local Functions");
                                builder.Add($"{Space}{Space}static void __Set<__TImpl>(ref __TImpl __impl, {propertyType} value) where __TImpl : {interfaceTypeString} => __impl.{propertyName} = value;");
                                builder.Add($"{Space}{Space}#endregion");
                                builder.Add($"{Space}}}");
                            }
                            else
                            {
                                builder.Add($"{Space}set => (({interfaceTypeString}){declaredSymbolName}).{propertyName} = value;");
                            }
                        }
                        else
                        {
                            builder.Add($"{Space}set => {declaredSymbolName}.{propertyName} = value;");
                        }
                    }

                    builder.Add($"}}");
                }
            }
        }

        return builder.ToImmutable();

        #region Local Functions
        static bool IsImplementingInterface(ITypeSymbol typeSymbol, ITypeSymbol interfaceSymbol)
        {
            return typeSymbol.Interfaces.Contains(interfaceSymbol, SymbolEqualityComparer.Default);
        }

        static IEnumerable<ISymbol> GetMembersWithBaseTypes(ITypeSymbol typeSymbol)
        {
            var overriddenSymbols = new HashSet<ISymbol>(SymbolEqualityComparer.Default);

            var baseTypeSymbol = typeSymbol;
            while (baseTypeSymbol != null && !IsBaseType(baseTypeSymbol))
            {
                foreach (var memberSymbol in baseTypeSymbol.GetMembers())
                {
                    if (memberSymbol.IsStatic)
                    {
                        continue;
                    }

                    switch (memberSymbol)
                    {
                        case IMethodSymbol { OverriddenMethod: { } overriddenMethod }:
                            overriddenSymbols.Add(overriddenMethod);
                            break;
                        case IPropertySymbol { OverriddenProperty: { } overriddenProperty }:
                            overriddenSymbols.Add(overriddenProperty);
                            break;
                    }

                    if (overriddenSymbols.Contains(memberSymbol))
                    {
                        continue;
                    }

                    yield return memberSymbol;
                }

                baseTypeSymbol = baseTypeSymbol.BaseType;
            }

            #region Local Functions
            static bool IsBaseType(ITypeSymbol symbol)
            {
                return symbol.ToDisplayString(FullyQualifiedFormat) is "object" or "global::System.ValueType";
            }
            #endregion
        }

        static (bool hasImplementedMember, bool isExplicit, bool isAbstract) GetImplementationContext(
            string mode,
            ITypeSymbol? containingTypeSymbol,
            ISymbol? implicitMemberSymbol,
            ISymbol? explicitMemberSymbol
        )
        {
            var defaultValue = (
                hasImplementedMember: false,
                isExplicit: false,
                isAbstract: false
            );

            var result = mode switch
            {
                Implicit => (implicitMemberSymbol, explicitMemberSymbol) switch
                {
                    (null, null) => defaultValue,
                    ({ IsAbstract: true }, null) => defaultValue with { isAbstract = true },
                    _ => defaultValue with { hasImplementedMember = true },
                },
                Explicit => explicitMemberSymbol == null
                    ? defaultValue with { isExplicit = true }
                    : defaultValue with { hasImplementedMember = true },
                Lift => implicitMemberSymbol switch
                {
                    null => defaultValue,
                    { IsAbstract: true } => defaultValue with { isAbstract = true },
                    _ => defaultValue with { hasImplementedMember = true },
                },
                _ => throw new InvalidOperationException($"Invalid mode: {mode}"),
            };

            var comparer = SymbolEqualityComparer.Default;
            return result.isAbstract && comparer.Equals(implicitMemberSymbol!.ContainingType, containingTypeSymbol)
                ? defaultValue with { hasImplementedMember = true }
                : result;
        }
        #endregion
    }

    private static ITypeSymbol GetDeclaredSymbolType(ISymbol symbol) => symbol switch
    {
        IFieldSymbol fieldSymbol => fieldSymbol.Type,
        IPropertySymbol propertySymbol => propertySymbol.Type,
        IParameterSymbol parameterSymbol => parameterSymbol.Type,
        _ => throw new InvalidOperationException($"Unexpected symbol type: {symbol.GetType().Name}"),
    };

    private static void AddSource(
        SourceProductionContext context,
        INamedTypeSymbol typeSymbol,
        ImmutableArray<string> lines
    )
    {
        if (lines.IsEmpty)
        {
            return;
        }

        var stringBuilder = CreateStringBuilderWithFileHeader();

        // begin namespace
        var hasNamespace = !typeSymbol.ContainingNamespace.IsGlobalNamespace;
        if (hasNamespace)
        {
            stringBuilder.AppendLine($"namespace {typeSymbol.ContainingNamespace.ToDisplayString()}");
            stringBuilder.AppendLine($"{{");
        }

        // get nestedTypes
        var nestedTypes = new List<INamedTypeSymbol>();
        var parentType = typeSymbol.ContainingType;
        while (parentType != null)
        {
            nestedTypes.Add(parentType);
            parentType = parentType.ContainingType;
        }

        var depthSpacerText = hasNamespace ? $"{Space}" : "";

        // begin nestedTypes
        for (var i = nestedTypes.Count - 1; i >= 0; --i)
        {
            var nestedType = nestedTypes[i];

            stringBuilder.AppendLine($"{depthSpacerText}{GetPartialTypeDeclarationString(nestedType)}");
            stringBuilder.AppendLine($"{depthSpacerText}{{");

            depthSpacerText += $"{Space}";
        }

        // begin containingType
        stringBuilder.AppendLine($"{depthSpacerText}{GetPartialTypeDeclarationString(typeSymbol)}");
        stringBuilder.AppendLine($"{depthSpacerText}{{");

        // generate factory methods
        depthSpacerText += $"{Space}";

        foreach (var line in lines)
        {
            stringBuilder.AppendLine($"{(line.Length > 0 ? depthSpacerText : "")}{line}");
        }

        depthSpacerText = depthSpacerText[..^4];

        // end containedType
        stringBuilder.AppendLine($"{depthSpacerText}}}");

        // end nestedTypes
        for (var i = 0; i < nestedTypes.Count; ++i)
        {
            depthSpacerText = depthSpacerText[..^4];

            stringBuilder.AppendLine($"{depthSpacerText}}}");
        }

        // end namespace
        if (hasNamespace)
        {
            stringBuilder.AppendLine($"}}");
        }

        context.AddSource(
            hintName: GetHintName(typeSymbol),
            sourceText: SourceText.From(stringBuilder.ToString(), Encoding.UTF8)
        );
    }
    #endregion

    #region IIncrementalGenerator Interface
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        IncrementalValuesProvider<(GenerationContext?, ImmutableArray<Diagnostic>)> valuesProvider = context
            .SyntaxProvider
            .CreateSyntaxProvider(
                predicate: static (syntaxNode, _) => IsCandidateMember(syntaxNode),
                transform: static (generatorSyntaxContext, _) => GetGenerationContexts(generatorSyntaxContext)
            )
            .SelectMany(static (generationContexts, _) => generationContexts);

        context.RegisterSourceOutput(valuesProvider.Collect(), (sourceProductionContext, generationContexts) =>
        {
            foreach (var diagnostic in generationContexts.SelectMany(tuple => tuple.Item2))
            {
                sourceProductionContext.ReportDiagnostic(diagnostic);
            }

            foreach (var pair in generationContexts
                .Where(generationContext => generationContext.Item1 != null)
                .Select(generationContext => ((GenerationContext, ImmutableArray<Diagnostic>))generationContext!)
                .GroupBy(
                    keySelector: generationContext => generationContext.Item1.DeclaredSymbol.ContainingType,
                    comparer: SymbolEqualityComparer.Default
                )
            )
            {
                var delegatedInterfaces = new HashSet<ITypeSymbol>(SymbolEqualityComparer.Default);
                var builder = ImmutableArray.CreateBuilder<string>();

                foreach (var (generationContext, _) in pair)
                {
                    if (generationContext is GenerationInterfaceContext &&
                        !delegatedInterfaces.Add(generationContext.DelegationTypeSymbol)
                    )
                    {
                        sourceProductionContext.ReportDiagnostic(Diagnostic.Create(
                            descriptor: DuplicateDelegationTargetRule,
                            location: generationContext.Attribute.ApplicationSyntaxReference?.GetSyntax().GetLocation(),
                            messageArgs: [generationContext.DelegationTypeSymbol]
                        ));

                        continue;
                    }

                    var lines = GenerateDelegationCode(generationContext);
                    if (lines.IsEmpty)
                    {
                        continue;
                    }

                    if (builder.Count > 0)
                    {
                        builder.Add("");
                    }

                    builder.Add($"#region {generationContext.DelegationTypeSymbol.ToDisplayString(FullyQualifiedFormat)}");
                    builder.AddRange(lines);
                    builder.Add("#endregion");
                }

                AddSource(
                    context: sourceProductionContext,
                    typeSymbol: (INamedTypeSymbol)pair.Key!,
                    lines: builder.ToImmutable()
                );
            }
        });

        #region Local Functions
        static bool IsCandidateMember(SyntaxNode node)
        {
            switch (node)
            {
                case FieldDeclarationSyntax { AttributeLists.Count: > 0 }:
                case PropertyDeclarationSyntax { AttributeLists.Count: > 0 }:
                case ParameterSyntax { AttributeLists.Count: > 0 } syntax when IsPrimaryConstructorParameter(syntax):
                    return true;
                default:
                    return false;
            }
        }

        static bool IsPrimaryConstructorParameter(ParameterSyntax parameter)
        {
            return parameter.Parent is ParameterListSyntax
            {
                Parent: RecordDeclarationSyntax or ClassDeclarationSyntax or StructDeclarationSyntax,
            };
        }
        #endregion
    }
    #endregion
}
