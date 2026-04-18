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
    private const string Space = "    ";
    #endregion

    #region Static
    private static ImmutableArray<string> GenerateDelegationCode(GenerationContext context)
    {
        var (
            _,
            declaredSymbol,
            delegationTypeSymbol
        ) = context;

        var isLiftMode = context is GenerationLiftContext;
        var isMemberImplementingInterface = DelegationMemberHelpers.IsMemberImplementingInterface(context);
        var isField = declaredSymbol is IFieldSymbol;

        var typeSymbol = declaredSymbol.ContainingType;
        var declaredSymbolName = declaredSymbol.Name;
        var interfaceTypeString = isLiftMode ? "" : delegationTypeSymbol.ToDisplayString(FullyQualifiedFormat);

        var getImplementedMember = BuildMemberComparer(typeSymbol, delegationTypeSymbol);
        var builder = ImmutableArray.CreateBuilder<string>();

        foreach (var symbol in DelegationMemberHelpers.GetTargetMembers(context))
        {
            var memberContext = DelegationMemberHelpers.CreateMemberGenerationContext(
                context,
                symbol,
                getImplementedMember
            );
            if (memberContext == null)
            {
                continue;
            }

            var (memberSymbol, symbolName, isExplicit, isAbstract, accessibility, @interface) = memberContext.Value;
            var @override = isAbstract ? "override " : "";

            if (memberSymbol is IMethodSymbol { MethodKind: Ordinary } methodSymbol)
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
                        builder.Add($"{Space}{(returnType != "void" ? "return " : "")}__{methodName}(in {declaredSymbolName}{(arguments.Length > 0 ? $", {arguments}" : "")});");
                        builder.Add($"");
                        builder.Add($"{Space}#region Local Functions");
                        builder.Add($"{Space}static {returnType} __{methodName}<__T>(in __T __impl{(parameters.Length > 0 ? $", {parameters}" : "")}) where __T : {interfaceTypeString} => __impl.{methodName}{genericParameters}({arguments});");
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
            else if (memberSymbol is IPropertySymbol propertySymbol)
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
                                builder.Add($"{Space}{Space}return __Get(in {declaredSymbolName}, {arguments});");
                                builder.Add($"");
                                builder.Add($"{Space}{Space}#region Local Functions");
                                builder.Add($"{Space}{Space}static {propertyType} __Get<__TImpl>(in __TImpl __impl, {parameters}) where __TImpl : {interfaceTypeString} => __impl[{arguments}];");
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
                                builder.Add($"{Space}{Space}__Set(in {declaredSymbolName}, {arguments}, value);");
                                builder.Add($"");
                                builder.Add($"{Space}{Space}#region Local Functions");
                                builder.Add($"{Space}{Space}static void __Set<__TImpl>(in __TImpl __impl, {parameters}, {propertyType} value) where __TImpl : {interfaceTypeString} => __impl[{arguments}] = value;");
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
                                builder.Add($"{Space}{Space}return __Get(in {declaredSymbolName});");
                                builder.Add($"");
                                builder.Add($"{Space}{Space}#region Local Functions");
                                builder.Add($"{Space}{Space}static {propertyType} __Get<__TImpl>(in __TImpl __impl) where __TImpl : {interfaceTypeString} => __impl.{propertyName};");
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
                                builder.Add($"{Space}{Space}__Set(in {declaredSymbolName}, value);");
                                builder.Add($"");
                                builder.Add($"{Space}{Space}#region Local Functions");
                                builder.Add($"{Space}{Space}static void __Set<__TImpl>(in __TImpl __impl, {propertyType} value) where __TImpl : {interfaceTypeString} => __impl.{propertyName} = value;");
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
            else if (memberSymbol is IEventSymbol eventSymbol)
            {
                if (builder.Count > 0)
                {
                    builder.Add("");
                }

                var eventType = eventSymbol.Type.ToDisplayString(FullyQualifiedFormat.WithMiscellaneousOptions(
                    IncludeNullableReferenceTypeModifier | UseSpecialTypes
                ));
                var eventName = eventSymbol.Name;

                builder.Add($"{accessibility}{@override}event {eventType} {@interface}{symbolName}");
                builder.Add("{");

                if (isMemberImplementingInterface)
                {
                    if (isField)
                    {
                        builder.Add($"{Space}add");
                        builder.Add($"{Space}{{");
                        builder.Add($"{Space}{Space}__Add(in {declaredSymbolName}, value);");
                        builder.Add("");
                        builder.Add($"{Space}{Space}#region Local Functions");
                        builder.Add($"{Space}{Space}static void __Add<__TImpl>(in __TImpl __impl, {eventType} value) where __TImpl : {interfaceTypeString} => __impl.{eventName} += value;");
                        builder.Add($"{Space}{Space}#endregion");
                        builder.Add($"{Space}}}");

                        builder.Add($"{Space}remove");
                        builder.Add($"{Space}{{");
                        builder.Add($"{Space}{Space}__Remove(in {declaredSymbolName}, value);");
                        builder.Add("");
                        builder.Add($"{Space}{Space}#region Local Functions");
                        builder.Add($"{Space}{Space}static void __Remove<__TImpl>(in __TImpl __impl, {eventType} value) where __TImpl : {interfaceTypeString} => __impl.{eventName} -= value;");
                        builder.Add($"{Space}{Space}#endregion");
                        builder.Add($"{Space}}}");
                    }
                    else
                    {
                        builder.Add($"{Space}add => (({interfaceTypeString}){declaredSymbolName}).{eventName} += value;");
                        builder.Add($"{Space}remove => (({interfaceTypeString}){declaredSymbolName}).{eventName} -= value;");
                    }
                }
                else
                {
                    builder.Add($"{Space}add => {declaredSymbolName}.{eventName} += value;");
                    builder.Add($"{Space}remove => {declaredSymbolName}.{eventName} -= value;");
                }

                builder.Add("}");
            }
        }

        return builder.ToImmutable();
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
        IncrementalValuesProvider<(GenerationContext?, ImmutableArray<Diagnostic>)> valuesProvider = context
            .SyntaxProvider
            .CreateSyntaxProvider(
                predicate: static (syntaxNode, _) => IsCandidateMember(syntaxNode),
                transform: static (generatorSyntaxContext, _) => GenerationContextFactory.Create(generatorSyntaxContext)
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
                            descriptor: GenerationDiagnostics.DuplicateDelegationTargetRule,
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
