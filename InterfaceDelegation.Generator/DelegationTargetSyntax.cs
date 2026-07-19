using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Macaron.InterfaceDelegation;

internal static class DelegationTargetSyntax
{
    public static bool IsSupported(SyntaxNode syntaxNode)
    {
        return syntaxNode switch
        {
            FieldDeclarationSyntax { Declaration.Variables.Count: 1 } => true,
            VariableDeclaratorSyntax
            {
                Parent: VariableDeclarationSyntax { Variables.Count: 1 },
            } => true,
            PropertyDeclarationSyntax => true,
            ParameterSyntax
            {
                Parent: ParameterListSyntax
                {
                    Parent: RecordDeclarationSyntax or ClassDeclarationSyntax or StructDeclarationSyntax,
                },
            } => true,
            _ => false,
        };
    }
}
