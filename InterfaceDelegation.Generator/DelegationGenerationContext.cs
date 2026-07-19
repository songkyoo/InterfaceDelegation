using Microsoft.CodeAnalysis;

namespace Macaron.InterfaceDelegation;

internal readonly record struct DelegationGenerationContext(
    DelegationDispatch Dispatch,
    MemberImplementationIndex ImplementationIndex
)
{
    public static DelegationGenerationContext Create(GenerationContext generationContext)
    {
        var declaredSymbol = generationContext.DeclaredSymbol;
        var typeSymbol = declaredSymbol.ContainingType;
        var delegationTypeSymbol = generationContext.DelegationTypeSymbol;

        return new DelegationGenerationContext(
            Dispatch: CreateDispatch(generationContext),
            ImplementationIndex: MemberComparisonHelper.CreateImplementationIndex(typeSymbol, delegationTypeSymbol)
        );
    }

    private static DelegationDispatch CreateDispatch(GenerationContext generationContext)
    {
        var declaredSymbol = generationContext.DeclaredSymbol;

        if (generationContext is not ExposeGenerationContext exposeContext
            || !ExposeGenerationPolicy.RequiresInterfaceDispatch(exposeContext)
        )
        {
            return new DirectDelegationDispatch(declaredSymbol.Name);
        }

        return declaredSymbol is IFieldSymbol
            ? new ConstrainedFieldDelegationDispatch(
                declaredSymbol.Name,
                exposeContext.DelegationTypeSymbol
            )
            : new InterfaceCastDelegationDispatch(
                declaredSymbol.Name,
                exposeContext.DelegationTypeSymbol
            );
    }
}
