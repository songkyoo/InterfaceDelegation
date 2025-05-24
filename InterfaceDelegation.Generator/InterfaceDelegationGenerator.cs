using System.Collections.Immutable;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

using static Macaron.InterfaceDelegation.MethodSignatureGenerationHelpers;
using static Macaron.InterfaceDelegation.MemberComparisonHelpers;
using static Macaron.InterfaceDelegation.SourceGenerationHelpers;
using static Microsoft.CodeAnalysis.SymbolDisplayFormat;

namespace Macaron.InterfaceDelegation;

[Generator]
public class InterfaceDelegationGenerator : IIncrementalGenerator
{
    #region Constants
    private const string Auto = nameof(ImplementationMode.Auto);
    private const string Implicit = nameof(ImplementationMode.Implicit);
    private const string Explicit = nameof(ImplementationMode.Explicit);

    private const string ImplementationOfAttributeString = "Macaron.InterfaceDelegation.ImplementationOfAttribute";

    private const string Space = "    ";
    #endregion

    #region Types
    private sealed record GenerationContext(
        ISymbol MemberSymbol,
        INamedTypeSymbol InterfaceTypeSymbol,
        string Mode
    );
    #endregion

    #region Static
    private static ImmutableArray<GenerationContext> GetGenerationContext(GeneratorSyntaxContext context)
    {
        var memberSymbol = context.Node switch
        {
            FieldDeclarationSyntax { Declaration.Variables: [var decl] } => GetDeclaredSymbol(decl),
            PropertyDeclarationSyntax decl => GetDeclaredSymbol(decl),
            ParameterSyntax decl => GetDeclaredSymbol(decl),
            _ => null,
        };
        if (memberSymbol?.ContainingType.TypeKind is not TypeKind.Class and not TypeKind.Struct ||
            memberSymbol is IPropertySymbol { Type.IsValueType: true }
        )
        {
            return ImmutableArray<GenerationContext>.Empty;
        }

        var builder = ImmutableArray.CreateBuilder<GenerationContext>();
        foreach (var attributeData in memberSymbol.GetAttributes())
        {
            var constructorArguments = attributeData.ConstructorArguments;

            if (attributeData.AttributeClass?.ToDisplayString() != ImplementationOfAttributeString ||
                constructorArguments.Length < 1
            )
            {
                continue;
            }

            if (constructorArguments[0].Value is INamedTypeSymbol interfaceTypeSymbol)
            {
                builder.Add(new GenerationContext(
                    MemberSymbol: memberSymbol,
                    InterfaceTypeSymbol: interfaceTypeSymbol,
                    Mode: GetImplementationMode(constructorArguments)
                ));
            }
        }

        return builder.ToImmutable();

        #region Local Functions
        static string GetImplementationMode(ImmutableArray<TypedConstant> constructorArguments)
        {
            var modeValue = constructorArguments.Length > 1
                ? (int)constructorArguments[1].Value!
                : 0;

            return modeValue switch
            {
                1 => Implicit,
                2 => Explicit,
                _ => Auto,
            };
        }

        ISymbol? GetDeclaredSymbol(SyntaxNode node) => context.SemanticModel.GetDeclaredSymbol(node);
        #endregion
    }

    private static ImmutableArray<string> GenerateInterfaceDelegationCode(GenerationContext context)
    {
        var (
            memberSymbol,
            interfaceTypeSymbol,
            mode
        ) = context;

        var memberTypeSymbol = memberSymbol switch
        {
            IFieldSymbol fieldSymbol => fieldSymbol.Type,
            IPropertySymbol propertySymbol => propertySymbol.Type,
            IParameterSymbol parameterSymbol => parameterSymbol.Type,
            _ => throw new InvalidOperationException($"Unexpected symbol type: {memberSymbol.GetType().Name}"),
        };
        var isMemberImplementingInterface = IsImplementingInterface(memberTypeSymbol, interfaceTypeSymbol);

        var typeSymbol = memberSymbol.ContainingType;
        var typeName = typeSymbol.Name;
        var memberName = memberSymbol.Name;

        var interfaceTypeString = interfaceTypeSymbol.ToDisplayString(FullyQualifiedFormat);

        var isField = memberSymbol is IFieldSymbol;
        var getImplementedMember = BuildMemberComparer(typeSymbol, interfaceTypeSymbol);
        var builder = ImmutableArray.CreateBuilder<string>();

        foreach (var symbol in interfaceTypeSymbol.GetMembers())
        {
            var (
                hasImplementedMember,
                isExplicit,
                isAbstract
            ) = GetImplementationContext(
                mode: symbol.Name == typeName ? Explicit : mode,
                containingTypeSymbol: typeSymbol,
                implicitMemberSymbol: getImplementedMember(symbol, false),
                explicitMemberSymbol: getImplementedMember(symbol, true)
            );

            if (hasImplementedMember)
            {
                continue;
            }

            var accessibility = isExplicit ? "" : "public ";
            var @override = isAbstract ? "override " : "";
            var @interface = isExplicit
                ? $"{interfaceTypeSymbol.ToDisplayString(FullyQualifiedFormat)}."
                : "";

            if (symbol is IMethodSymbol { MethodKind: MethodKind.Ordinary } methodSymbol)
            {
                var genericParameterNames = methodSymbol.TypeParameters.Length > 0
                    ? string.Join(", ", methodSymbol.TypeParameters.Select(static symbol => symbol.Name))
                    : "";
                var genericParameterConstraints = methodSymbol
                    .TypeParameters
                    .Select(GetTypeParameterConstraintClause)
                    .Where(static constraint => constraint.Length > 0)
                    .ToImmutableArray();

                var returnType = methodSymbol.ReturnType.ToDisplayString(FullyQualifiedFormat);
                var methodName = methodSymbol.Name;
                var genericParameters = genericParameterNames.Length > 0 ? $"<{genericParameterNames}>" : "";
                var parameters = string.Join(", ", methodSymbol.Parameters.Select(GetParameterString));
                var arguments = string.Join(", ", methodSymbol.Parameters.Select(GetArgumentString));

                if (builder.Count > 0)
                {
                    builder.Add("");
                }

                builder.Add($"{accessibility}{@override}{returnType} {@interface}{methodName}{genericParameters}({parameters})");

                foreach (var constraint in genericParameterConstraints)
                {
                    builder.Add($"{Space}{constraint}");
                }

                if (isMemberImplementingInterface)
                {
                    builder.Add($"{{");

                    if (isField)
                    {
                        builder.Add($"{Space}{(returnType != "void" ? "return " : "")}__{methodName}(ref {memberName}{(arguments.Length > 0 ? $", {arguments}" : "")});");
                        builder.Add($"");
                        builder.Add($"{Space}#region Local Functions");
                        builder.Add($"{Space}static {returnType} __{methodName}<__T>(ref __T __impl{(parameters.Length > 0 ? $", {parameters}" : "")}) where __T : {interfaceTypeString} => __impl.{methodName}{genericParameters}({arguments});");
                        builder.Add($"{Space}#endregion");
                    }
                    else
                    {
                        builder.Add($"{Space}{(returnType != "void" ? "return " : "")}(({interfaceTypeString}){memberName}).{methodName}({(arguments.Length > 0 ? $"{arguments}" : "")});");
                    }

                    builder.Add($"}}");
                }
                else
                {
                    builder.Add($"{Space}=> {memberName}.{methodName}{genericParameters}({arguments});");
                }
            }
            else if (symbol is IPropertySymbol propertySymbol)
            {
                if (propertySymbol.SetMethod?.IsInitOnly is true)
                {
                    continue;
                }

                if (builder.Count > 0)
                {
                    builder.Add("");
                }

                var propertyType = propertySymbol.Type.ToDisplayString(FullyQualifiedFormat);
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
                                builder.Add($"{Space}{Space}return __Get(ref {memberName}, {arguments});");
                                builder.Add($"");
                                builder.Add($"{Space}{Space}#region Local Functions");
                                builder.Add($"{Space}{Space}static {propertyType} __Get<__TImpl>(ref __TImpl __impl, {parameters}) where __TImpl : {interfaceTypeString} => __impl[{arguments}];");
                                builder.Add($"{Space}{Space}#endregion");
                                builder.Add($"{Space}}}");
                            }
                            else
                            {
                                builder.Add($"{Space}get => (({interfaceTypeString}){memberName})[{arguments}];");
                            }
                        }
                        else
                        {
                            builder.Add($"{Space}get => {memberName}[{arguments}];");
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
                                builder.Add($"{Space}{Space}__Set(ref {memberName}, {arguments}, value);");
                                builder.Add($"");
                                builder.Add($"{Space}{Space}#region Local Functions");
                                builder.Add($"{Space}{Space}static void __Set<__TImpl>(ref __TImpl __impl, {parameters}, {propertyType} value) where __TImpl : {interfaceTypeString} => __impl[{arguments}] = value;");
                                builder.Add($"{Space}{Space}#endregion");
                                builder.Add($"{Space}}}");
                            }
                            else
                            {
                                builder.Add($"{Space}set => (({interfaceTypeString}){memberName})[{arguments}] = value;");
                            }
                        }
                        else
                        {
                            builder.Add($"{Space}set => {memberName}[{arguments}] = value;");
                        }
                    }

                    builder.Add($"}}");
                }
                else
                {
                    builder.Add($"{accessibility}{@override}{propertyType} {@interface}{propertyName}");
                    builder.Add($"{{");

                    if (propertySymbol.GetMethod != null)
                    {
                        if (isMemberImplementingInterface)
                        {
                            if (isField)
                            {
                                builder.Add($"{Space}get");
                                builder.Add($"{Space}{{");
                                builder.Add($"{Space}{Space}return __Get(ref {memberName});");
                                builder.Add($"");
                                builder.Add($"{Space}{Space}#region Local Functions");
                                builder.Add($"{Space}{Space}static {propertyType} __Get<__TImpl>(ref __TImpl __impl) where __TImpl : {interfaceTypeString} => __impl.{propertyName};");
                                builder.Add($"{Space}{Space}#endregion");
                                builder.Add($"{Space}}}");
                            }
                            else
                            {
                                builder.Add($"{Space}get => (({interfaceTypeString}){memberName}).{propertyName};");
                            }
                        }
                        else
                        {
                            builder.Add($"{Space}get => {memberName}.{propertyName};");
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
                                builder.Add($"{Space}{Space}__Set(ref {memberName}, value);");
                                builder.Add($"");
                                builder.Add($"{Space}{Space}#region Local Functions");
                                builder.Add($"{Space}{Space}static void __Set<__TImpl>(ref __TImpl __impl, {propertyType} value) where __TImpl : {interfaceTypeString} => __impl.{propertyName} = value;");
                                builder.Add($"{Space}{Space}#endregion");
                                builder.Add($"{Space}}}");
                            }
                            else
                            {
                                builder.Add($"{Space}set => (({interfaceTypeString}){memberName}).{propertyName} = value;");
                            }
                        }
                        else
                        {
                            builder.Add($"{Space}set => {memberName}.{propertyName} = value;");
                        }
                    }

                    builder.Add($"}}");
                }
            }
        }

        return builder.ToImmutable();

        #region Local Functions
        static bool IsImplementingInterface(ITypeSymbol typeSymbol, INamedTypeSymbol interfaceSymbol)
        {
            return typeSymbol.Interfaces.Contains(interfaceSymbol, SymbolEqualityComparer.Default);
        }

        static (bool hasImplementedMember, bool isExplicit, bool isAbstract) GetImplementationContext(
            string mode,
            INamedTypeSymbol? containingTypeSymbol,
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
                _ => (implicitMemberSymbol, explicitMemberSymbol) switch
                {
                    (null, null) => defaultValue,
                    ({ IsAbstract: true }, null) => defaultValue with { isAbstract = true },
                    _ => defaultValue with { hasImplementedMember = true },
                }
            };

            return
                result.isAbstract &&
                SymbolEqualityComparer.Default.Equals(implicitMemberSymbol!.ContainingType, containingTypeSymbol)
                ? defaultValue with { hasImplementedMember = true }
                : result;
        }
        #endregion
    }

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
        IncrementalValuesProvider<GenerationContext> valuesProvider = context
            .SyntaxProvider
            .CreateSyntaxProvider(
                predicate: static (syntaxNode, _) => IsCandidateMember(syntaxNode),
                transform: static (generatorSyntaxContext, _) => GetGenerationContext(generatorSyntaxContext)
            )
            .SelectMany(static (generationContexts, _) => generationContexts);

        context.RegisterSourceOutput(valuesProvider.Collect(), (sourceProductionContext, generationContexts) =>
        {
            foreach (var pair in generationContexts.GroupBy(
                keySelector: generationContext => generationContext.MemberSymbol.ContainingType,
                comparer: SymbolEqualityComparer.Default
            ))
            {
                var builder = ImmutableArray.CreateBuilder<string>();

                foreach (var generationContext in pair)
                {
                    var lines = GenerateInterfaceDelegationCode(generationContext);
                    if (lines.IsEmpty)
                    {
                        continue;
                    }

                    if (builder.Count > 0)
                    {
                        builder.Add("");
                    }

                    builder.Add($"#region Implementation of {generationContext.InterfaceTypeSymbol.ToDisplayString(FullyQualifiedFormat)}");
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
