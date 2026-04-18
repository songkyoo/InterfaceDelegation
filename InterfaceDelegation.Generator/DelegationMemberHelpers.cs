using Microsoft.CodeAnalysis;

using static Microsoft.CodeAnalysis.Accessibility;
using static Microsoft.CodeAnalysis.SymbolDisplayFormat;

namespace Macaron.InterfaceDelegation;

internal static class DelegationMemberHelpers
{
    private const string Implicit = nameof(ImplementationMode.Implicit);
    private const string Explicit = nameof(ImplementationMode.Explicit);
    private const string Lift = nameof(Lift);

    internal readonly record struct MemberGenerationContext(
        ISymbol Symbol,
        string SymbolName,
        bool IsExplicit,
        bool IsAbstract,
        string Accessibility,
        string InterfacePrefix
    );

    public static bool IsMemberImplementingInterface(GenerationContext context)
    {
        if (context is GenerationLiftContext)
        {
            return false;
        }

        return GenerationContextFactory
            .GetDeclaredSymbolType(context.DeclaredSymbol)
            .Interfaces
            .Contains(context.DelegationTypeSymbol, SymbolEqualityComparer.Default);
    }

    public static IEnumerable<ISymbol> GetTargetMembers(GenerationContext context)
    {
        var includeBaseTypes = context is not GenerationLiftContext liftContext || liftContext.IncludeBaseTypes;
        foreach (var symbol in includeBaseTypes
            ? GetMembersWithBaseTypes(context.DelegationTypeSymbol)
            : GetMembers(context.DelegationTypeSymbol)
        )
        {
            if (context is GenerationLiftContext liftContext2 && !ShouldIncludeLiftSymbol(liftContext2, symbol))
            {
                continue;
            }

            yield return symbol;
        }
    }

    public static MemberGenerationContext? CreateMemberGenerationContext(
        GenerationContext context,
        ISymbol symbol,
        Func<ISymbol, string, bool, bool, ISymbol?> getImplementedMember
    )
    {
        var typeSymbol = context.DeclaredSymbol.ContainingType;
        var symbolName = GetGeneratedSymbolName(context, symbol);
        var mode = GetGenerationMode(context, symbolName, typeSymbol.Name);
        var checkReturnType = context is not GenerationLiftContext;

        var (
            hasImplementedMember,
            isExplicit,
            isAbstract
        ) = GetImplementationContext(
            mode: mode,
            containingTypeSymbol: typeSymbol,
            implicitMemberSymbol: getImplementedMember(symbol, symbolName, false, checkReturnType),
            explicitMemberSymbol: getImplementedMember(symbol, symbolName, true, checkReturnType)
        );

        if (hasImplementedMember)
        {
            return null;
        }

        return new MemberGenerationContext(
            Symbol: symbol,
            SymbolName: symbolName,
            IsExplicit: isExplicit,
            IsAbstract: isAbstract,
            Accessibility: GetAccessibilityPrefix(context, symbol, isExplicit),
            InterfacePrefix: isExplicit
                ? $"{context.DelegationTypeSymbol.ToDisplayString(FullyQualifiedFormat)}."
                : ""
        );
    }

    private static string GetGeneratedSymbolName(GenerationContext context, ISymbol symbol)
    {
        if (context is GenerationLiftContext liftContext &&
            liftContext.Rename.TryGetValue(symbol.Name, out var renamed))
        {
            return renamed;
        }

        return symbol.Name;
    }

    private static string GetGenerationMode(GenerationContext context, string symbolName, string typeName)
    {
        return context switch
        {
            GenerationLiftContext => Lift,
            GenerationInterfaceContext { Mode: ImplementationMode.Explicit } => Explicit,
            _ when symbolName == typeName => Explicit,
            _ => Implicit,
        };
    }

    private static string GetAccessibilityPrefix(GenerationContext context, ISymbol symbol, bool isExplicit)
    {
        if (isExplicit)
        {
            return "";
        }

        return context is GenerationLiftContext
            ? $"{symbol.DeclaredAccessibility.ToString().ToLower()} "
            : "public ";
    }

    private static bool ShouldIncludeLiftSymbol(GenerationLiftContext context, ISymbol symbol)
    {
        if (symbol.DeclaredAccessibility is not Public and not Internal)
        {
            return false;
        }

        if (!context.Filter.IsEmpty && !context.Filter.Contains(symbol.Name))
        {
            return false;
        }

        return !context.Remove.Contains(symbol.Name);
    }

    private static IEnumerable<ISymbol> GetMembersWithBaseTypes(ITypeSymbol typeSymbol)
    {
        if (typeSymbol.TypeKind == TypeKind.Interface)
        {
            foreach (var memberSymbol in new[] { typeSymbol }.Concat(typeSymbol.AllInterfaces)
                .SelectMany(symbol => symbol.GetMembers())
                .Where(symbol => !symbol.IsStatic)
            )
            {
                yield return memberSymbol;
            }

            yield break;
        }

        var overriddenSymbols = new HashSet<ISymbol>(SymbolEqualityComparer.Default);
        var baseTypeSymbol = typeSymbol;
        while (baseTypeSymbol != null && !IsBaseType(baseTypeSymbol))
        {
            foreach (var memberSymbol in baseTypeSymbol.GetMembers())
            {
                if (memberSymbol.IsStatic)
                {
                    continue;
                }

                switch (memberSymbol)
                {
                    case IMethodSymbol { OverriddenMethod: { } overriddenMethod }:
                        overriddenSymbols.Add(overriddenMethod);
                        break;
                    case IPropertySymbol { OverriddenProperty: { } overriddenProperty }:
                        overriddenSymbols.Add(overriddenProperty);
                        break;
                    case IEventSymbol { OverriddenEvent: { } overriddenEvent }:
                        overriddenSymbols.Add(overriddenEvent);
                        break;
                }

                if (overriddenSymbols.Contains(memberSymbol))
                {
                    continue;
                }

                yield return memberSymbol;
            }

            baseTypeSymbol = baseTypeSymbol.BaseType;
        }
    }

    private static IEnumerable<ISymbol> GetMembers(ITypeSymbol typeSymbol)
    {
        foreach (var memberSymbol in typeSymbol.GetMembers())
        {
            if (!memberSymbol.IsStatic)
            {
                yield return memberSymbol;
            }
        }
    }

    private static (bool hasImplementedMember, bool isExplicit, bool isAbstract) GetImplementationContext(
        string mode,
        ITypeSymbol? containingTypeSymbol,
        ISymbol? implicitMemberSymbol,
        ISymbol? explicitMemberSymbol
    )
    {
        var defaultValue = (
            hasImplementedMember: false,
            isExplicit: false,
            isAbstract: false
        );

        var result = mode switch
        {
            Implicit => (implicitMemberSymbol, explicitMemberSymbol) switch
            {
                (null, null) => defaultValue,
                ({ IsAbstract: true }, null) => defaultValue with { isAbstract = true },
                _ => defaultValue with { hasImplementedMember = true },
            },
            Explicit => explicitMemberSymbol == null
                ? defaultValue with { isExplicit = true }
                : defaultValue with { hasImplementedMember = true },
            Lift => implicitMemberSymbol switch
            {
                null => defaultValue,
                { IsAbstract: true } => defaultValue with { isAbstract = true },
                _ => defaultValue with { hasImplementedMember = true },
            },
            _ => throw new InvalidOperationException($"Invalid mode: {mode}"),
        };

        var comparer = SymbolEqualityComparer.Default;
        return result.isAbstract && comparer.Equals(implicitMemberSymbol!.ContainingType, containingTypeSymbol)
            ? defaultValue with { hasImplementedMember = true }
            : result;
    }

    private static bool IsBaseType(ITypeSymbol symbol)
    {
        return symbol.ToDisplayString(FullyQualifiedFormat) is "object" or "global::System.ValueType";
    }
}
