using System.Collections.Immutable;

namespace Macaron.InterfaceDelegation;

internal static class DelegationCodeGenerator
{
    public static ImmutableArray<string> Generate(GenerationContext context)
    {
        return context switch
        {
            GenerationInterfaceContext interfaceContext => GenerateExpose(interfaceContext),
            GenerationLiftContext liftContext => GenerateLift(liftContext),
            _ => ImmutableArray<string>.Empty,
        };
    }

    private static ImmutableArray<string> GenerateExpose(GenerationInterfaceContext context)
    {
        var executionContext = DelegationExecutionContext.Create(context);
        var builder = ImmutableArray.CreateBuilder<string>();

        foreach (var symbol in ExposeDelegationPolicy.GetTargetMembers(context))
        {
            var memberContext = ExposeDelegationPolicy.CreateMemberGenerationContext(
                context,
                symbol,
                executionContext.GetImplementedMember
            );
            if (memberContext == null)
            {
                continue;
            }

            ExposeDelegationRendering.TryRenderMember(
                DelegationRenderingHelpers.RenderContext.Create(executionContext, memberContext.Value),
                builder
            );
        }

        return builder.ToImmutable();
    }

    private static ImmutableArray<string> GenerateLift(GenerationLiftContext context)
    {
        var executionContext = DelegationExecutionContext.Create(context);
        var builder = ImmutableArray.CreateBuilder<string>();

        foreach (var symbol in LiftDelegationPolicy.GetTargetMembers(context))
        {
            var memberContext = LiftDelegationPolicy.CreateMemberGenerationContext(
                context,
                symbol,
                executionContext.GetImplementedMember
            );
            if (memberContext == null)
            {
                continue;
            }

            LiftDelegationRendering.TryRenderMember(
                DelegationRenderingHelpers.RenderContext.Create(executionContext, memberContext.Value),
                builder
            );
        }

        return builder.ToImmutable();
    }
}
