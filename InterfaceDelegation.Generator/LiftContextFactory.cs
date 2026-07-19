using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

namespace Macaron.InterfaceDelegation;

internal static class LiftContextFactory
{
    public static ImmutableArray<GenerationAnalysisResult> CreateAll(
        GeneratorAttributeSyntaxContext context,
        CancellationToken cancellationToken
    )
    {
        cancellationToken.ThrowIfCancellationRequested();

        if (!DelegationTargetSymbol.IsSupported(context.TargetSymbol))
        {
            return ImmutableArray<GenerationAnalysisResult>.Empty;
        }

        var builder = ImmutableArray.CreateBuilder<GenerationAnalysisResult>(context.Attributes.Length);

        foreach (var attributeData in context.Attributes.OrderBy(GetAttributeSpanStart))
        {
            cancellationToken.ThrowIfCancellationRequested();

            builder.Add(Create(attributeData, context.TargetSymbol));
        }

        return builder.ToImmutable();
    }

    private static GenerationAnalysisResult Create(AttributeData attributeData, ISymbol declaredSymbol)
    {
        var constructorArguments = attributeData.ConstructorArguments;
        var includeBaseTypes = constructorArguments[0].Value is true;
        var filter = ReadStringArray(constructorArguments[1]).ToImmutableHashSet();
        var remove = ReadStringArray(constructorArguments[2]).ToImmutableHashSet();
        var rename = ReadStringArray(constructorArguments[3])
            .Where(static value => !string.IsNullOrWhiteSpace(value))
            .Select(ParseRename)
            .Where(static pair => pair != null)
            .Select(static pair => pair!.Value)
            .ToImmutableDictionary();
        var delegationTypeSymbol = DelegationTargetSymbol.GetDeclaredType(declaredSymbol);
        var hasMemberOptions = !filter.IsEmpty || !remove.IsEmpty || !rename.IsEmpty;
        var precomputedTargetMembers = hasMemberOptions
            ? GetConfigurableMembers(delegationTypeSymbol, includeBaseTypes).ToImmutableArray()
            : default;

        return new GenerationAnalysisResult(
            Context: new LiftGenerationContext(
                Attribute: attributeData,
                DeclaredSymbol: declaredSymbol,
                DelegationTypeSymbol: delegationTypeSymbol,
                IncludeBaseTypes: includeBaseTypes,
                Filter: filter,
                Remove: remove,
                Rename: rename,
                PrecomputedTargetMembers: precomputedTargetMembers
            ),
            Diagnostics: hasMemberOptions
                ? CreateOptionDiagnostics(attributeData, delegationTypeSymbol, precomputedTargetMembers)
                : ImmutableArray<Diagnostic>.Empty
        );
    }

    private static string[] ReadStringArray(TypedConstant constant)
    {
        return !constant.IsNull
            ? constant.Values.Select(static value => (string?)value.Value ?? "").ToArray()
            : [];
    }

    private static KeyValuePair<string, string>? ParseRename(string value)
    {
        var values = value.Split(':').Select(static part => part.Trim()).ToArray();

        return values.Length != 2 || values.Any(static part => part.Length < 1)
            ? null
            : new KeyValuePair<string, string>(values[0], values[1]);
    }

    private static IEnumerable<ISymbol> GetConfigurableMembers(
        ITypeSymbol delegationTypeSymbol,
        bool includeBaseTypes
    )
    {
        var members = includeBaseTypes
            ? DelegationMemberHelper.GetMembersWithBaseTypes(delegationTypeSymbol)
            : DelegationMemberHelper.GetMembers(delegationTypeSymbol);

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

    private static ImmutableArray<Diagnostic> CreateOptionDiagnostics(
        AttributeData attributeData,
        ITypeSymbol delegationTypeSymbol,
        ImmutableArray<ISymbol> targetMembers
    )
    {
        var availableMemberNames = targetMembers
            .Select(static symbol => symbol.Name)
            .ToImmutableHashSet();
        var builder = ImmutableArray.CreateBuilder<Diagnostic>();

        AddMissingMemberDiagnostics(
            builder,
            attributeData,
            delegationTypeSymbol,
            availableMemberNames,
            parameterName: "filter"
        );
        AddMissingMemberDiagnostics(
            builder,
            attributeData,
            delegationTypeSymbol,
            availableMemberNames,
            parameterName: "remove"
        );
        AddMissingMemberDiagnostics(
            builder,
            attributeData,
            delegationTypeSymbol,
            availableMemberNames,
            parameterName: "rename",
            getMemberName: static value => ParseRename(value)?.Key
        );

        return builder.ToImmutable();
    }

    private static void AddMissingMemberDiagnostics(
        ImmutableArray<Diagnostic>.Builder builder,
        AttributeData attributeData,
        ITypeSymbol delegationTypeSymbol,
        ImmutableHashSet<string> availableMemberNames,
        string parameterName,
        Func<string, string?>? getMemberName = null
    )
    {
        getMemberName ??= static value => value;

        foreach (var (value, location) in AttributeArgumentReader.ReadStringArguments(attributeData, parameterName))
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

    private static int GetAttributeSpanStart(AttributeData attributeData)
    {
        return attributeData.ApplicationSyntaxReference?.Span.Start ?? int.MaxValue;
    }
}
