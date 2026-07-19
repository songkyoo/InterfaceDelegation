using Microsoft.CodeAnalysis;

namespace Macaron.InterfaceDelegation;

internal static class ExposeMemberRules
{
    public static bool IsSupportedInterfaceMember(ISymbol symbol)
    {
        if (symbol.IsStatic || symbol.DeclaredAccessibility != Accessibility.Public)
        {
            return false;
        }

        return symbol switch
        {
            IMethodSymbol { MethodKind: MethodKind.Ordinary } => true,
            IPropertySymbol => true,
            IEventSymbol => true,
            _ => false,
        };
    }
}
