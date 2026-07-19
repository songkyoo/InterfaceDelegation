using System.Collections.Immutable;

namespace Macaron.InterfaceDelegation;

internal abstract class DelegationDispatchRenderer
{
    protected const string Space = "    ";

    public static DelegationDispatchRenderer Create(DelegationDispatch dispatch)
    {
        return dispatch switch
        {
            DirectDelegationDispatch directDispatch => new DirectDelegationDispatchRenderer(directDispatch),
            InterfaceCastDelegationDispatch castDispatch => new InterfaceCastDelegationDispatchRenderer(
                castDispatch
            ),
            ConstrainedFieldDelegationDispatch fieldDispatch => new ConstrainedFieldDelegationDispatchRenderer(
                fieldDispatch
            ),
            _ => throw new ArgumentOutOfRangeException(nameof(dispatch), dispatch, null),
        };
    }

    public abstract void RenderMethod(
        string returnType,
        string methodName,
        string genericParameters,
        string parameters,
        string arguments,
        ImmutableArray<string>.Builder builder
    );

    public abstract void RenderPropertyGetter(
        string propertyType,
        string propertyName,
        ImmutableArray<string>.Builder builder
    );

    public abstract void RenderPropertySetter(
        string propertyType,
        string propertyName,
        ImmutableArray<string>.Builder builder
    );

    public abstract void RenderIndexerGetter(
        string propertyType,
        string parameters,
        string arguments,
        ImmutableArray<string>.Builder builder
    );

    public abstract void RenderIndexerSetter(
        string propertyType,
        string parameters,
        string arguments,
        ImmutableArray<string>.Builder builder
    );

    public abstract void RenderEvent(
        string eventType,
        string eventName,
        ImmutableArray<string>.Builder builder
    );
}
