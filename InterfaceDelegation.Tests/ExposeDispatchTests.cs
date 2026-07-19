using Microsoft.CodeAnalysis;
using static Macaron.InterfaceDelegation.Tests.Helper;

namespace Macaron.InterfaceDelegation.Tests;

[TestFixture]
public sealed class ExposeDispatchTests
{
    [Test]
    public void UsesNominalContractFastPath_ForDefaultInterfaceImplementation()
    {
        const string sourceCode =
            """
            namespace Macaron.InterfaceDelegation.Tests;

            public interface IFoo
            {
                void Run() { }
            }

            public sealed class Foo : IFoo
            {
            }

            public partial class Wrapper : IFoo
            {
                [Expose(typeof(IFoo))]
                private readonly Foo _impl = new();
            }
            """;

        var result = RunGenerator(sourceCode);

        AssertSuccessfulGeneration(result);
        Assert.Multiple(() =>
        {
            Assert.That(result.Diagnostics, Has.None.Matches<Diagnostic>(static diagnostic => diagnostic.Id == "MAID0005"));
            Assert.That(result.GeneratedSources, Has.Length.EqualTo(1));
            Assert.That(result.GeneratedSources[0].SourceText.ToString(), Does.Contain("where __T : global::Macaron.InterfaceDelegation.Tests.IFoo"));
        });
    }

    [Test]
    public void UsesInterfaceDispatch_WhenBaseTypeImplementsInterfaceExplicitly()
    {
        const string sourceCode =
            """
            namespace Macaron.InterfaceDelegation.Tests;

            public interface IFoo
            {
                void Run();
            }

            public class FooBase : IFoo
            {
                void IFoo.Run() { }
            }

            public sealed class Foo : FooBase
            {
            }

            public partial class Wrapper : IFoo
            {
                [Expose(typeof(IFoo))]
                private readonly Foo _impl = new();
            }
            """;

        var result = RunGenerator(sourceCode);

        AssertSuccessfulGeneration(result);
        Assert.Multiple(() =>
        {
            Assert.That(result.Diagnostics, Has.None.Matches<Diagnostic>(static diagnostic => diagnostic.Id == "MAID0005"));
            Assert.That(result.GeneratedSources, Has.Length.EqualTo(1));
            Assert.That(result.GeneratedSources[0].SourceText.ToString(), Does.Contain("__Run(in _impl)"));
        });
    }

    [Test]
    public void UsesIndexedContractValidation_ForDuckTypedTarget()
    {
        const string sourceCode =
            """
            namespace Macaron.InterfaceDelegation.Tests;

            public interface IFoo
            {
                void Run(int value);

                int Value { get; }

                int Map(int value);

                string Map(string value);
            }

            public sealed class DuckFoo
            {
                public void Run(int value) { }

                public int Value => 42;

                public int Map(int value) => value;

                public string Map(string value) => value;
            }

            public partial class Wrapper : IFoo
            {
                [Expose(typeof(IFoo))]
                private readonly DuckFoo _impl = new();
            }
            """;

        var result = RunGenerator(sourceCode);

        AssertSuccessfulGeneration(result);
        Assert.Multiple(() =>
        {
            Assert.That(result.Diagnostics, Has.None.Matches<Diagnostic>(static diagnostic => diagnostic.Id == "MAID0005"));
            Assert.That(result.GeneratedSources, Has.Length.EqualTo(1));
            Assert.That(result.GeneratedSources[0].SourceText.ToString(), Does.Contain("=> _impl.Run(value);"));
        });
    }

    [Test]
    public void IgnoresPrivateDefaultInterfaceHelpers()
    {
        const string sourceCode =
            """
            namespace Macaron.InterfaceDelegation.Tests;

            public interface IFoo
            {
                void Run();

                private void Helper() { }
            }

            public sealed class Foo : IFoo
            {
                public void Run() { }
            }

            public partial class Wrapper : IFoo
            {
                [Expose(typeof(IFoo))]
                private readonly Foo _impl = new();
            }
            """;

        var result = RunGenerator(sourceCode);

        AssertSuccessfulGeneration(result);
        Assert.Multiple(() =>
        {
            Assert.That(result.Diagnostics, Has.None.Matches<Diagnostic>(static diagnostic => diagnostic.Id == "MAID0005"));
            Assert.That(result.GeneratedSources, Has.Length.EqualTo(1));
            Assert.That(result.GeneratedSources[0].SourceText.ToString(), Does.Contain("__Run(in _impl)"));
            Assert.That(result.GeneratedSources[0].SourceText.ToString(), Does.Not.Contain("Helper"));
        });
    }
}

