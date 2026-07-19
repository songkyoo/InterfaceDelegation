using Microsoft.CodeAnalysis;

using static Microsoft.CodeAnalysis.Accessibility;
using static Microsoft.CodeAnalysis.SymbolDisplayFormat;
using static Microsoft.CodeAnalysis.TypeKind;

namespace Macaron.InterfaceDelegation;

public static class MemberComparisonHelper
{
    public static bool ImplementsInterface(ITypeSymbol typeSymbol, ITypeSymbol interfaceSymbol)
    {
        var comparer = SymbolEqualityComparer.Default;

        return
            comparer.Equals(typeSymbol, interfaceSymbol)
            || typeSymbol.AllInterfaces.Contains(interfaceSymbol, comparer);
    }

    public static Func<ISymbol, bool> BuildCompatibleImplementationChecker(
        ITypeSymbol typeSymbol,
        ITypeSymbol interfaceSymbol,
        Func<ISymbol, bool>? isAccessible = null
    )
    {
        var memberSymbols = GetComparableMembers(typeSymbol);

        if (isAccessible != null)
        {
            memberSymbols = memberSymbols.Where(isAccessible);
        }

        var explicitImplementations = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
        var implementationIndex = MemberImplementationIndex.Create(
            memberSymbols,
            interfaceSymbol,
            explicitImplementations
        );

        return interfaceMemberSymbol =>
            implementationIndex.FindImplicit(
                interfaceMemberSymbol,
                interfaceMemberSymbol.Name,
                checkReturnType: true
            ) != null
            || explicitImplementations.Contains(interfaceMemberSymbol);
    }

    internal static MemberImplementationIndex CreateImplementationIndex(
        ITypeSymbol typeSymbol,
        ITypeSymbol interfaceSymbol
    )
    {
        return MemberImplementationIndex.Create(GetMembersWithBaseTypes(typeSymbol), interfaceSymbol);
    }

    internal static bool MatchesMethodSignature(
        IMethodSymbol methodSymbol,
        string methodName,
        IMethodSymbol targetMethodSymbol,
        bool checkReturnType
    )
    {
        var comparer = SymbolEqualityComparer.Default;

        if (methodName != targetMethodSymbol.Name)
        {
            return false;
        }

        if (checkReturnType && !comparer.Equals(methodSymbol.ReturnType, targetMethodSymbol.ReturnType))
        {
            return false;
        }

        if (methodSymbol.Parameters.Length != targetMethodSymbol.Parameters.Length)
        {
            return false;
        }

        if (methodSymbol.Arity != targetMethodSymbol.Arity)
        {
            return false;
        }

        for (var i = 0; i < methodSymbol.Parameters.Length; i++)
        {
            var parameterSymbol = methodSymbol.Parameters[i];
            var targetParameterSymbol = targetMethodSymbol.Parameters[i];

            if (!comparer.Equals(parameterSymbol.Type, targetParameterSymbol.Type))
            {
                return false;
            }

            if (parameterSymbol.RefKind != targetParameterSymbol.RefKind)
            {
                return false;
            }

            if (parameterSymbol.IsParams != targetParameterSymbol.IsParams)
            {
                return false;
            }
        }

        return true;
    }

    internal static bool MatchesPropertySignature(
        IPropertySymbol propertySymbol,
        string propertyName,
        IPropertySymbol targetPropertySymbol
    )
    {
        var comparer = SymbolEqualityComparer.Default;

        if (!propertyName.Equals(targetPropertySymbol.Name)
            || !comparer.Equals(propertySymbol.Type, targetPropertySymbol.Type)
            || propertySymbol.Parameters.Length != targetPropertySymbol.Parameters.Length
        )
        {
            return false;
        }

        for (var i = 0; i < propertySymbol.Parameters.Length; i++)
        {
            var parameterSymbol = propertySymbol.Parameters[i];
            var targetParameterSymbol = targetPropertySymbol.Parameters[i];

            if (!comparer.Equals(parameterSymbol.Type, targetParameterSymbol.Type)
                || parameterSymbol.RefKind != targetParameterSymbol.RefKind
            )
            {
                return false;
            }
        }

        if (propertySymbol.GetMethod != null && targetPropertySymbol.GetMethod == null)
        {
            return false;
        }

        return propertySymbol.SetMethod == null || targetPropertySymbol.SetMethod != null;
    }

    internal static bool MatchesEventSignature(
        IEventSymbol eventSymbol,
        string eventName,
        IEventSymbol targetEventSymbol
    )
    {
        var comparer = SymbolEqualityComparer.Default;

        return
            eventName.Equals(targetEventSymbol.Name)
            && comparer.Equals(eventSymbol.Type, targetEventSymbol.Type);
    }

    private static IEnumerable<ISymbol> GetMembersWithBaseTypes(ITypeSymbol typeSymbol)
    {
        foreach (var memberSymbol in typeSymbol.GetMembers())
        {
            yield return memberSymbol;
        }

        var baseTypeSymbol = typeSymbol.BaseType;

        while (baseTypeSymbol != null && !IsSystemType(baseTypeSymbol))
        {
            foreach (var memberSymbol in baseTypeSymbol
                .GetMembers()
                .Where(symbol => symbol.DeclaredAccessibility != Private)
            )
            {
                yield return memberSymbol;
            }

            baseTypeSymbol = baseTypeSymbol.BaseType;
        }

        #region Local Functions
        static bool IsSystemType(ITypeSymbol symbol)
        {
            return symbol.ToDisplayString(FullyQualifiedFormat) is "object" or "global::System.ValueType";
        }
        #endregion
    }

    private static IEnumerable<ISymbol> GetComparableMembers(ITypeSymbol typeSymbol)
    {
        if (typeSymbol.TypeKind == Interface)
        {
            foreach (var memberSymbol in DelegationMemberUtilities.GetMembersWithBaseTypes(typeSymbol))
            {
                if (!memberSymbol.IsStatic)
                {
                    yield return memberSymbol;
                }
            }

            yield break;
        }

        foreach (var memberSymbol in GetMembersWithBaseTypes(typeSymbol))
        {
            if (!memberSymbol.IsStatic)
            {
                yield return memberSymbol;
            }
        }
    }
}
