using Microsoft.CodeAnalysis;

using static Macaron.InterfaceDelegation.MemberComparisonHelpers;
using static Microsoft.CodeAnalysis.SymbolDisplayFormat;

namespace Macaron.InterfaceDelegation;

internal readonly record struct DelegationExecutionContext(
    GenerationContext GenerationContext,
    ISymbol DeclaredSymbol,
    INamedTypeSymbol TypeSymbol,
    bool IsLiftMode,
    bool IsMemberImplementingInterface,
    bool IsField,
    string DeclaredSymbolName,
    string InterfaceTypeString,
    Func<ISymbol, string, bool, bool, ISymbol?> GetImplementedMember
)
{
    public static DelegationExecutionContext Create(GenerationContext generationContext)
    {
        var declaredSymbol = generationContext.DeclaredSymbol;
        var typeSymbol = declaredSymbol.ContainingType;
        var delegationTypeSymbol = generationContext.DelegationTypeSymbol;
        var isLiftMode = generationContext is GenerationLiftContext;

        return new DelegationExecutionContext(
            GenerationContext: generationContext,
            DeclaredSymbol: declaredSymbol,
            TypeSymbol: typeSymbol,
            IsLiftMode: isLiftMode,
            IsMemberImplementingInterface: DelegationMemberHelpers.IsMemberImplementingInterface(generationContext),
            IsField: declaredSymbol is IFieldSymbol,
            DeclaredSymbolName: declaredSymbol.Name,
            InterfaceTypeString: isLiftMode ? "" : delegationTypeSymbol.ToDisplayString(FullyQualifiedFormat),
            GetImplementedMember: BuildMemberComparer(typeSymbol, delegationTypeSymbol)
        );
    }
}
