using Microsoft.CodeAnalysis;

using static Macaron.InterfaceDelegation.MemberComparisonHelper;
using static Microsoft.CodeAnalysis.SymbolDisplayFormat;

namespace Macaron.InterfaceDelegation;

internal readonly record struct DelegationGenerationContext(
    bool IsMemberImplementingInterface,
    bool IsField,
    string DeclaredSymbolName,
    string InterfaceTypeString,
    Func<ISymbol, string, bool, bool, ISymbol?> GetImplementedMember
)
{
    public static DelegationGenerationContext Create(GenerationContext generationContext)
    {
        var declaredSymbol = generationContext.DeclaredSymbol;
        var typeSymbol = declaredSymbol.ContainingType;
        var delegationTypeSymbol = generationContext.DelegationTypeSymbol;
        var isLiftMode = generationContext is GenerationLiftContext;
        var isMemberImplementingInterface = generationContext switch
        {
            GenerationInterfaceContext interfaceContext => ExposeGenerationPolicy.IsMemberImplementingInterface(
                interfaceContext
            ),
            _ => false,
        };

        return new DelegationGenerationContext(
            IsMemberImplementingInterface: isMemberImplementingInterface,
            IsField: declaredSymbol is IFieldSymbol,
            DeclaredSymbolName: declaredSymbol.Name,
            InterfaceTypeString: isLiftMode ? "" : delegationTypeSymbol.ToDisplayString(FullyQualifiedFormat),
            GetImplementedMember: BuildMemberComparer(typeSymbol, delegationTypeSymbol)
        );
    }
}
