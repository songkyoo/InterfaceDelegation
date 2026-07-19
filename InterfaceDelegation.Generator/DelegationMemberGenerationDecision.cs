namespace Macaron.InterfaceDelegation;

internal enum DelegationMemberGenerationDecision
{
    Generate,
    GenerateExplicitInterfaceImplementation,
    OverrideAbstractMember,
    Skip,
}
