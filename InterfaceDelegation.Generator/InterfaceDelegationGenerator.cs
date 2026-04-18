using System.Collections.Immutable;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

using static Macaron.InterfaceDelegation.MemberComparisonHelpers;
using static Macaron.InterfaceDelegation.SourceGenerationHelpers;
using static Microsoft.CodeAnalysis.SymbolDisplayFormat;

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
        var executionContext = DelegationExecutionContext.Create(context);
        var builder = ImmutableArray.CreateBuilder<string>();

        foreach (var symbol in DelegationMemberHelpers.GetTargetMembers(context))
        {
            var memberContext = DelegationMemberHelpers.CreateMemberGenerationContext(
                executionContext.GenerationContext,
                symbol,
                executionContext.GetImplementedMember
            );
            if (memberContext == null)
            {
                continue;
            }

            DelegationRenderingHelpers.TryRenderMember(
                new DelegationRenderingHelpers.RenderContext(
                    ExecutionContext: executionContext,
                    MemberContext: memberContext.Value,
                    IsLiftMode: executionContext.IsLiftMode,
                    IsMemberImplementingInterface: executionContext.IsMemberImplementingInterface,
                    IsField: executionContext.IsField,
                    DeclaredSymbolName: executionContext.DeclaredSymbolName,
                    InterfaceTypeString: executionContext.InterfaceTypeString
                ),
                builder
            );
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
