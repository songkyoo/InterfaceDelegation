using System.Collections.Immutable;

namespace Macaron.InterfaceDelegation;

internal static class DelegationGenerationPipeline
{
    public static ImmutableArray<string> Generate(GenerationContext context)
    {
        return context switch
        {
            ExposeGenerationContext exposeContext => GenerateExpose(exposeContext),
            LiftGenerationContext liftContext => GenerateLift(liftContext),
            _ => ImmutableArray<string>.Empty,
        };
    }

    private static ImmutableArray<string> GenerateExpose(ExposeGenerationContext context)
    {
        var executionContext = DelegationGenerationContext.Create(context);
        var builder = ImmutableArray.CreateBuilder<string>();

        foreach (var symbol in ExposeGenerationPolicy.GetTargetMembers(context))
        {
            var memberContext = ExposeGenerationPolicy.CreateMemberGenerationContext(
                context,
                symbol,
                executionContext.GetImplementedMember
            );
            if (memberContext == null)
            {
                continue;
            }

            ExposeRenderingPolicy.RenderMember(
                DelegationRenderingCore.RenderContext.Create(executionContext, memberContext.Value),
                builder
            );
        }

        return builder.ToImmutable();
    }

    private static ImmutableArray<string> GenerateLift(LiftGenerationContext context)
    {
        var executionContext = DelegationGenerationContext.Create(context);
        var builder = ImmutableArray.CreateBuilder<string>();

        foreach (var symbol in LiftGenerationPolicy.GetTargetMembers(context))
        {
            var memberContext = LiftGenerationPolicy.CreateMemberGenerationContext(
                context,
                symbol,
                executionContext.GetImplementedMember
            );
            if (memberContext == null)
            {
                continue;
            }

            LiftRenderingPolicy.RenderMember(
                DelegationRenderingCore.RenderContext.Create(executionContext, memberContext.Value),
                builder
            );
        }

        return builder.ToImmutable();
    }
}
