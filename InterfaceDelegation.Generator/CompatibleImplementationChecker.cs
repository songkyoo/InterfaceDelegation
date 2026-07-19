using Microsoft.CodeAnalysis;

namespace Macaron.InterfaceDelegation;

internal sealed class CompatibleImplementationChecker(
    MemberImplementationIndex implementationIndex,
    HashSet<ISymbol> explicitImplementations
)
{
    public bool HasImplementation(ISymbol interfaceMemberSymbol)
    {
        return implementationIndex.FindImplicit(
                interfaceMemberSymbol,
                interfaceMemberSymbol.Name,
                MethodReturnTypeComparison.Match
            ) != null
            || explicitImplementations.Contains(interfaceMemberSymbol);
    }
}
