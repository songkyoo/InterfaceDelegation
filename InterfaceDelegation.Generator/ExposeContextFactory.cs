using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

namespace Macaron.InterfaceDelegation;

internal static class ExposeContextFactory
{
    public static GenerationAnalysisResult Create(
        AttributeData attributeData,
        ISymbol declaredSymbol,
        Compilation compilation,
        CancellationToken cancellationToken = default
    )
    {
        var constructorArguments = attributeData.ConstructorArguments;

        if (declaredSymbol is IPropertySymbol { Type.IsValueType: true })
        {
            return new GenerationAnalysisResult(
                Context: null,
                Diagnostics: ImmutableArray.Create(Diagnostic.Create(
                    descriptor: GenerationDiagnostics.ValueTypePropertyCannotBeDelegatedRule,
                    location: declaredSymbol.Locations.FirstOrDefault(),
                    messageArgs: [declaredSymbol.Name]
                ))
            );
        }

        var interfaceTypeSymbol = GetInterfaceType(attributeData, declaredSymbol);

        if (interfaceTypeSymbol == null)
        {
            return new GenerationAnalysisResult(
                Context: null,
                Diagnostics: ImmutableArray.Create(Diagnostic.Create(
                    descriptor: GenerationDiagnostics.InvalidImplementationTargetRule,
                    location: AttributeArgumentReader.GetFirstArgumentLocation(attributeData),
                    messageArgs: [constructorArguments[0].Value]
                ))
            );
        }

        if (interfaceTypeSymbol.TypeKind is not TypeKind.Interface || interfaceTypeSymbol.IsUnboundGenericType)
        {
            return new GenerationAnalysisResult(
                Context: null,
                Diagnostics: ImmutableArray.Create(Diagnostic.Create(
                    descriptor: GenerationDiagnostics.InvalidImplementationTargetRule,
                    location: AttributeArgumentReader.GetFirstArgumentLocation(attributeData),
                    messageArgs: [interfaceTypeSymbol.ToDisplayString()]
                ))
            );
        }

        var contractDiagnostics = CreateContractDiagnostics(
            attributeData,
            DelegationTargetSymbol.GetDeclaredType(declaredSymbol),
            interfaceTypeSymbol,
            declaredSymbol.ContainingType,
            compilation,
            cancellationToken
        );

        if (!contractDiagnostics.IsEmpty)
        {
            return new GenerationAnalysisResult(null, contractDiagnostics);
        }

        return new GenerationAnalysisResult(
            Context: new ExposeGenerationContext(
                Attribute: attributeData,
                DeclaredSymbol: declaredSymbol,
                DelegationTypeSymbol: interfaceTypeSymbol,
                Mode: GetImplementationMode(constructorArguments)
            ),
            Diagnostics: ImmutableArray<Diagnostic>.Empty
        );
    }

    private static INamedTypeSymbol? GetInterfaceType(AttributeData attributeData, ISymbol declaredSymbol)
    {
        var constructorArgument = attributeData.ConstructorArguments[0].Value;

        return constructorArgument == null
            ? DelegationTargetSymbol.GetDeclaredType(declaredSymbol) as INamedTypeSymbol
            : constructorArgument as INamedTypeSymbol;
    }

    private static ImmutableArray<Diagnostic> CreateContractDiagnostics(
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
        var location = AttributeArgumentReader.GetFirstArgumentLocation(attributeData) ??
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

        foreach (var interfaceMember in DelegationMemberProvider.GetMembersIncludingBaseTypes(interfaceTypeSymbol))
        {
            cancellationToken.ThrowIfCancellationRequested();

            if (!ExposeMemberRules.IsSupportedInterfaceMember(interfaceMember))
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

    private static ImplementationMode GetImplementationMode(ImmutableArray<TypedConstant> constructorArguments)
    {
        return (ImplementationMode)(constructorArguments[1].Value ?? 0) switch
        {
            var value and ImplementationMode.Explicit => value,
            _ => ImplementationMode.Implicit,
        };
    }
}
