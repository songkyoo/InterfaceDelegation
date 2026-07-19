using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Macaron.InterfaceDelegation;

internal static class DelegationTargetSymbol
{
    public static bool IsSupported(ISymbol symbol)
    {
        if (symbol.ContainingType?.TypeKind is not TypeKind.Class and not TypeKind.Struct)
        {
            return false;
        }

        return symbol switch
        {
            IFieldSymbol fieldSymbol => fieldSymbol.DeclaringSyntaxReferences.Any(static syntaxReference =>
                syntaxReference.GetSyntax() is VariableDeclaratorSyntax
                {
                    Parent: VariableDeclarationSyntax { Variables.Count: 1 },
                }
            ),
            IPropertySymbol propertySymbol => propertySymbol.DeclaringSyntaxReferences.Any(static syntaxReference =>
                syntaxReference.GetSyntax() is PropertyDeclarationSyntax
            ),
            IParameterSymbol parameterSymbol => parameterSymbol.DeclaringSyntaxReferences.Any(static syntaxReference =>
                syntaxReference.GetSyntax() is ParameterSyntax
                {
                    Parent: ParameterListSyntax
                    {
                        Parent: RecordDeclarationSyntax or ClassDeclarationSyntax or StructDeclarationSyntax,
                    },
                }
            ),
            _ => false,
        };
    }

    public static ITypeSymbol GetDeclaredType(ISymbol symbol)
    {
        return symbol switch
        {
            IFieldSymbol fieldSymbol => fieldSymbol.Type,
            IPropertySymbol propertySymbol => propertySymbol.Type,
            IParameterSymbol parameterSymbol => parameterSymbol.Type,
            _ => throw new InvalidOperationException($"Unexpected symbol type: {symbol.GetType().Name}"),
        };
    }
}
