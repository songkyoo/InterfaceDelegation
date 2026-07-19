using Microsoft.CodeAnalysis;
using static Macaron.InterfaceDelegation.Tests.Helper;

namespace Macaron.InterfaceDelegation.Tests;

[TestFixture]
public sealed class GeneratorDiagnosticTests
{
    [Test]
    public void ReportsDiagnostic_When_ExposeAttributeAppliedToValueTypeProperty()
    {
        const string sourceCode =
            """
            namespace Macaron.InterfaceDelegation.Tests;

            public interface IFoo
            {
                int GetValue();
            }

            public partial class TestClass : IFoo
            {
                [Expose(typeof(IFoo))]
                private int Impl { get; } = 42;
            }
            """;

        var (diagnostics, _) = CompileAndGetResults(sourceCode);

        Assert.That(diagnostics, Has.Some.Matches<Diagnostic>(diagnostic => diagnostic.Id == "MAID0002"));
    }

    [Test]
    public void ReportsDiagnostic_When_InterfaceIsDelegatedMoreThanOnce()
    {
        const string sourceCode =
            """
            namespace Macaron.InterfaceDelegation.Tests;

            public interface IFoo
            {
                void Bar();
            }

            public class FooImpl : IFoo
            {
                public void Bar() { }
            }

            public partial class TestClass : IFoo
            {
                [Expose(typeof(IFoo))]
                private readonly IFoo _impl1 = new FooImpl();

                [Expose(typeof(IFoo))]
                private readonly IFoo _impl2 = new FooImpl();
            }
            """;

        var (diagnostics, _) = CompileAndGetResults(sourceCode);

        Assert.That(diagnostics, Has.Some.Matches<Diagnostic>(diagnostic => diagnostic.Id == "MAID0003"));
    }

    [Test]
    public void ReportsDiagnostic_When_LiftOptionReferencesMissingMember()
    {
        const string sourceCode =
            """
            namespace Macaron.InterfaceDelegation.Tests;

            public class LiftTarget
            {
                public void Existing() { }
            }

            public partial class TestClass
            {
                [Lift(
                    filter: new[] { "MissingFilter" },
                    remove: new[] { "MissingRemove" },
                    rename: new[] { "MissingRename:Renamed" }
                )]
                private readonly LiftTarget _impl = new();
            }
            """;

        var (diagnostics, _) = CompileAndGetResults(sourceCode);
        var maid0004Diagnostics = diagnostics
            .Where(diagnostic => diagnostic.Id == "MAID0004")
            .OrderBy(diagnostic => diagnostic.Location.SourceSpan.Start)
            .ToArray();

        Assert.That(maid0004Diagnostics, Has.Length.EqualTo(3));
        Assert.That(maid0004Diagnostics.Select(diagnostic => diagnostic.GetMessage()), Is.EqualTo(new[]
        {
            "The member 'MissingFilter' was not found on 'Macaron.InterfaceDelegation.Tests.LiftTarget' for Lift option 'filter'",
            "The member 'MissingRemove' was not found on 'Macaron.InterfaceDelegation.Tests.LiftTarget' for Lift option 'remove'",
            "The member 'MissingRename' was not found on 'Macaron.InterfaceDelegation.Tests.LiftTarget' for Lift option 'rename'",
        }));
        Assert.That(maid0004Diagnostics.Select(diagnostic => diagnostic.Location.SourceSpan.Start), Is.EqualTo(new[]
        {
            sourceCode.IndexOf("\"MissingFilter\"", StringComparison.Ordinal),
            sourceCode.IndexOf("\"MissingRemove\"", StringComparison.Ordinal),
            sourceCode.IndexOf("\"MissingRename:Renamed\"", StringComparison.Ordinal),
        }));
    }

    [Test]
    public void ReportsDiagnostic_When_ExposeTargetDoesNotImplementInterfaceMembers()
    {
        const string sourceCode =
            """
            namespace Macaron.InterfaceDelegation.Tests;

            public interface IFoo
            {
                void Run(int value);

                int Count { get; }
            }

            public class FooImpl
            {
                public void Run() { }

                public string Count => "";
            }

            public partial class TestClass : IFoo
            {
                [Expose(typeof(IFoo))]
                private readonly FooImpl _impl = new();
            }
            """;

        var (diagnostics, generatedCode) = CompileAndGetResults(sourceCode);
        var maid0005Diagnostics = diagnostics
            .Where(diagnostic => diagnostic.Id == "MAID0005")
            .OrderBy(diagnostic => diagnostic.GetMessage(), StringComparer.Ordinal)
            .ToArray();
        var messages = maid0005Diagnostics
            .Select(diagnostic => diagnostic.GetMessage())
            .ToArray();

        Assert.That(generatedCode, Is.Empty);
        Assert.That(maid0005Diagnostics, Has.Length.EqualTo(2));
        Assert.That(messages.Any(message => message.Contains("FooImpl", StringComparison.Ordinal) && message.Contains("IFoo.Count", StringComparison.Ordinal)), Is.True);
        Assert.That(messages.Any(message => message.Contains("FooImpl", StringComparison.Ordinal) && message.Contains("Run", StringComparison.Ordinal)), Is.True);
    }

    [Test]
    public void ReportsDiagnostic_When_ExposeTargetEventSignatureDoesNotMatch()
    {
        const string sourceCode =
            """
            namespace Macaron.InterfaceDelegation.Tests;

            using System;

            public interface INotifier
            {
                event EventHandler? Changed;

                void Notify();
            }

            public class NotifierImpl
            {
                public event Action? Changed;

                public void Notify() { }
            }

            public partial class TestEventDelegation : INotifier
            {
                [Expose(typeof(INotifier))]
                private readonly NotifierImpl _impl = new NotifierImpl();
            }
            """;

        var (diagnostics, generatedCode) = CompileAndGetResults(sourceCode);
        var maid0005Diagnostics = diagnostics
            .Where(diagnostic => diagnostic.Id == "MAID0005")
            .ToArray();

        Assert.That(generatedCode, Is.Empty);
        Assert.That(maid0005Diagnostics, Has.Length.EqualTo(1));
        Assert.That(maid0005Diagnostics[0].GetMessage(), Does.Contain("INotifier.Changed"));
    }

    [Test]
    public void RejectsInaccessibleDuckTypedMembers()
    {
        const string sourceCode =
            """
            namespace Macaron.InterfaceDelegation.Tests;

            public interface IFoo
            {
                void Run();
            }

            public sealed class DuckFoo
            {
                private void Run() { }
            }

            public partial class Wrapper : IFoo
            {
                [Expose(typeof(IFoo))]
                private readonly DuckFoo _impl = new();
            }
            """;

        var (diagnostics, generatedCode) = CompileAndGetResults(sourceCode);

        Assert.Multiple(() =>
        {
            Assert.That(diagnostics, Has.Some.Matches<Diagnostic>(static diagnostic => diagnostic.Id == "MAID0005"));
            Assert.That(generatedCode, Is.Empty);
        });
    }
}
