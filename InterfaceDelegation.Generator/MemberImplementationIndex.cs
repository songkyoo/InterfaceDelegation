using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

namespace Macaron.InterfaceDelegation;

internal sealed class MemberImplementationIndex
{
    private readonly record struct MethodLookupKey(
        string Name,
        int Arity,
        int ParameterCount,
        int ParameterHash
    );

    private readonly record struct PropertyLookupKey(
        string Name,
        int ParameterCount,
        int SignatureHash
    );

    private readonly record struct EventLookupKey(
        string Name,
        int TypeHash
    );

    private sealed record MemberLookups(
        ImmutableDictionary<MethodLookupKey, ImmutableArray<IMethodSymbol>> Methods,
        ImmutableDictionary<PropertyLookupKey, ImmutableArray<IPropertySymbol>> Properties,
        ImmutableDictionary<EventLookupKey, ImmutableArray<IEventSymbol>> Events
    );

    private readonly MemberLookups _implicitMembers;
    private readonly MemberLookups _explicitMembers;

    public static MemberImplementationIndex Create(
        IEnumerable<ISymbol> memberSymbols,
        ITypeSymbol interfaceSymbol,
        HashSet<ISymbol>? explicitImplementations = null
    )
    {
        var symbolComparer = SymbolEqualityComparer.Default;
        var implicitMethods = new List<IMethodSymbol>();
        var implicitProperties = new List<IPropertySymbol>();
        var implicitEvents = new List<IEventSymbol>();
        var explicitMethods = new List<IMethodSymbol>();
        var explicitProperties = new List<IPropertySymbol>();
        var explicitEvents = new List<IEventSymbol>();

        foreach (var memberSymbol in memberSymbols)
        {
            switch (memberSymbol)
            {
                case IMethodSymbol
                {
                    MethodKind: not MethodKind.Constructor,
                }
                and
                {
                    MethodKind: MethodKind.ExplicitInterfaceImplementation,
                    ExplicitInterfaceImplementations: var explicitMethodSymbolsForMember,
                }:
                    {
                        foreach (var explicitMethodSymbol in explicitMethodSymbolsForMember)
                        {
                            explicitImplementations?.Add(explicitMethodSymbol);

                            if (symbolComparer.Equals(explicitMethodSymbol.ContainingType, interfaceSymbol))
                            {
                                explicitMethods.Add(explicitMethodSymbol);
                            }
                        }

                        break;
                    }
                case IMethodSymbol { MethodKind: not MethodKind.Constructor } methodSymbol:
                    {
                        if (methodSymbol.MethodKind == MethodKind.Ordinary)
                        {
                            implicitMethods.Add(methodSymbol);
                        }

                        break;
                    }
                case IPropertySymbol { ExplicitInterfaceImplementations.IsEmpty: false } propertySymbol:
                    {
                        foreach (var explicitPropertySymbol in propertySymbol.ExplicitInterfaceImplementations)
                        {
                            explicitImplementations?.Add(explicitPropertySymbol);

                            if (symbolComparer.Equals(explicitPropertySymbol.ContainingType, interfaceSymbol))
                            {
                                explicitProperties.Add(explicitPropertySymbol);
                            }
                        }

                        break;
                    }
                case IPropertySymbol propertySymbol:
                    {
                        implicitProperties.Add(propertySymbol);

                        break;
                    }
                case IEventSymbol { ExplicitInterfaceImplementations.IsEmpty: false } eventSymbol:
                    {
                        foreach (var explicitEventSymbol in eventSymbol.ExplicitInterfaceImplementations)
                        {
                            explicitImplementations?.Add(explicitEventSymbol);

                            if (symbolComparer.Equals(explicitEventSymbol.ContainingType, interfaceSymbol))
                            {
                                explicitEvents.Add(explicitEventSymbol);
                            }
                        }

                        break;
                    }
                case IEventSymbol eventSymbol:
                    {
                        implicitEvents.Add(eventSymbol);

                        break;
                    }
            }
        }

        return new MemberImplementationIndex(
            implicitMembers: new MemberLookups(
                Methods: ToLookup(implicitMethods, method => GetLookupKey(method, method.Name)),
                Properties: ToLookup(implicitProperties, property => GetLookupKey(property, property.Name)),
                Events: ToLookup(implicitEvents, @event => GetLookupKey(@event, @event.Name))
            ),
            explicitMembers: new MemberLookups(
                Methods: ToLookup(explicitMethods, method => GetLookupKey(method, method.Name)),
                Properties: ToLookup(explicitProperties, property => GetLookupKey(property, property.Name)),
                Events: ToLookup(explicitEvents, @event => GetLookupKey(@event, @event.Name))
            )
        );
    }

    public ISymbol? FindImplicit(
        ISymbol symbol,
        string symbolName,
        MethodReturnTypeComparison returnTypeComparison
    )
    {
        return Find(symbol, symbolName, returnTypeComparison, _implicitMembers);
    }

    public ISymbol? FindExplicit(
        ISymbol symbol,
        string symbolName,
        MethodReturnTypeComparison returnTypeComparison
    )
    {
        return Find(symbol, symbolName, returnTypeComparison, _explicitMembers);
    }

    private MemberImplementationIndex(MemberLookups implicitMembers, MemberLookups explicitMembers)
    {
        _implicitMembers = implicitMembers;
        _explicitMembers = explicitMembers;
    }

    private static ISymbol? Find(
        ISymbol symbol,
        string symbolName,
        MethodReturnTypeComparison returnTypeComparison,
        MemberLookups lookups
    )
    {
        switch (symbol)
        {
            case IMethodSymbol methodSymbol:
                {
                    if (lookups.Methods.TryGetValue(GetLookupKey(methodSymbol, symbolName), out var methods))
                    {
                        return methods.FirstOrDefault(candidate =>
                            MemberComparisonHelper.MatchesMethodSignature(
                                methodSymbol,
                                symbolName,
                                candidate,
                                returnTypeComparison
                            )
                        );
                    }

                    break;
                }
            case IPropertySymbol propertySymbol:
                {
                    if (lookups.Properties.TryGetValue(GetLookupKey(propertySymbol, symbolName), out var properties))
                    {
                        return properties.FirstOrDefault(candidate =>
                            MemberComparisonHelper.MatchesPropertySignature(propertySymbol, symbolName, candidate)
                        );
                    }

                    break;
                }
            case IEventSymbol eventSymbol:
                {
                    if (lookups.Events.TryGetValue(GetLookupKey(eventSymbol, symbolName), out var events))
                    {
                        return events.FirstOrDefault(candidate =>
                            MemberComparisonHelper.MatchesEventSignature(eventSymbol, symbolName, candidate)
                        );
                    }

                    break;
                }
        }

        return null;
    }

    private static MethodLookupKey GetLookupKey(IMethodSymbol methodSymbol, string methodName)
    {
        var parameterHash = 17;
        foreach (var parameterSymbol in methodSymbol.Parameters)
        {
            parameterHash = CombineHash(parameterHash, SymbolEqualityComparer.Default.GetHashCode(parameterSymbol.Type));
            parameterHash = CombineHash(parameterHash, (int)parameterSymbol.RefKind);
            parameterHash = CombineHash(parameterHash, parameterSymbol.IsParams ? 1 : 0);
        }

        return new MethodLookupKey(
            methodName,
            methodSymbol.Arity,
            methodSymbol.Parameters.Length,
            parameterHash
        );
    }

    private static PropertyLookupKey GetLookupKey(IPropertySymbol propertySymbol, string propertyName)
    {
        var signatureHash = SymbolEqualityComparer.Default.GetHashCode(propertySymbol.Type);
        foreach (var parameterSymbol in propertySymbol.Parameters)
        {
            signatureHash = CombineHash(signatureHash, SymbolEqualityComparer.Default.GetHashCode(parameterSymbol.Type));
            signatureHash = CombineHash(signatureHash, (int)parameterSymbol.RefKind);
        }

        return new PropertyLookupKey(propertyName, propertySymbol.Parameters.Length, signatureHash);
    }

    private static EventLookupKey GetLookupKey(IEventSymbol eventSymbol, string eventName)
    {
        return new EventLookupKey(eventName, SymbolEqualityComparer.Default.GetHashCode(eventSymbol.Type));
    }

    private static int CombineHash(int current, int value)
    {
        return unchecked((current * 31) + value);
    }

    private static ImmutableDictionary<TKey, ImmutableArray<TSymbol>> ToLookup<TKey, TSymbol>(
        IEnumerable<TSymbol> symbols,
        Func<TSymbol, TKey> getKey
    ) where TKey : notnull
    {
        return symbols
            .GroupBy(getKey)
            .ToImmutableDictionary(
                keySelector: grouping => grouping.Key,
                elementSelector: grouping => grouping.ToImmutableArray()
            );
    }
}
