using Microsoft.CodeAnalysis;

using static Microsoft.CodeAnalysis.SymbolDisplayFormat;

namespace Macaron.InterfaceDelegation;

internal readonly record struct DelegationGenerationContext(
    bool IsMemberImplementingInterface,
    bool IsField,
    string DeclaredSymbolName,
    string InterfaceTypeString,
    MemberImplementationIndex ImplementationIndex
)
{
    public static DelegationGenerationContext Create(GenerationContext generationContext)
    {
        var declaredSymbol = generationContext.DeclaredSymbol;
        var typeSymbol = declaredSymbol.ContainingType;
        var delegationTypeSymbol = generationContext.DelegationTypeSymbol;
        var isLiftMode = generationContext is LiftGenerationContext;
        var isMemberImplementingInterface = generationContext switch
        {
            ExposeGenerationContext exposeContext => ExposeGenerationPolicy.IsMemberImplementingInterface(
                exposeContext
            ),
            _ => false,
        };

        return new DelegationGenerationContext(
            IsMemberImplementingInterface: isMemberImplementingInterface,
            IsField: declaredSymbol is IFieldSymbol,
            DeclaredSymbolName: declaredSymbol.Name,
            InterfaceTypeString: isLiftMode ? "" : delegationTypeSymbol.ToDisplayString(FullyQualifiedFormat),
            ImplementationIndex: MemberComparisonHelper.CreateImplementationIndex(typeSymbol, delegationTypeSymbol)
        );
    }
}
