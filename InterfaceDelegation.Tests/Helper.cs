using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;

namespace Macaron.InterfaceDelegation.Tests;

internal static class Helper
{
    public sealed record GeneratorTestResult(
        ImmutableArray<Diagnostic> Diagnostics,
        ImmutableArray<GeneratedSourceResult> GeneratedSources,
        Compilation OutputCompilation
    );

    public static void AssertGeneratedCode(string sourceCode, string expected)
    {
        var result = RunGenerator(sourceCode);

        AssertSuccessfulGeneration(result);
        Assert.That(result.GeneratedSources, Has.Length.EqualTo(1));
        Assert.That(
            result.GeneratedSources[0].SourceText.ToString().ReplaceLineEndings(),
            Is.EqualTo(expected.ReplaceLineEndings())
        );
    }

    public static void AssertGeneratedCodes(string sourceCode, params string[] expected)
    {
        var result = RunGenerator(sourceCode);

        AssertSuccessfulGeneration(result);
        Assert.That(result.GeneratedSources, Has.Length.EqualTo(expected.Length));
        Assert.That(
            result.GeneratedSources.Select(static source => source.SourceText.ToString().ReplaceLineEndings()),
            Is.EqualTo(expected.Select(static source => source.ReplaceLineEndings()))
        );
    }

    public static void AssertSuccessfulGeneration(GeneratorTestResult result)
    {
        var errors = result
            .OutputCompilation
            .GetDiagnostics()
            .Where(static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error)
            .ToArray();

        Assert.That(errors, Is.Empty, string.Join(Environment.NewLine, errors.Select(static diagnostic => diagnostic.ToString())));
    }

    public static (ImmutableArray<Diagnostic> diagnostics, string generatedCode) CompileAndGetResults(string sourceCode)
    {
        var result = RunGenerator(sourceCode);
        var generatedCode = result.GeneratedSources.Length > 0
            ? result.GeneratedSources[0].SourceText.ToString()
            : "";

        return (result.Diagnostics, generatedCode);
    }

    public static GeneratorTestResult RunGenerator(string sourceCode)
    {
        var attributeAssembly = typeof(ExposeAttribute).Assembly;
        var references = AppDomain
            .CurrentDomain
            .GetAssemblies()
            .Where(assembly => !assembly.IsDynamic && !string.IsNullOrWhiteSpace(assembly.Location))
            .Append(attributeAssembly)
            .Select(assembly => MetadataReference.CreateFromFile(assembly.Location))
            .Cast<MetadataReference>()
            .ToImmutableArray();

        var syntaxTree = CSharpSyntaxTree.ParseText(sourceCode);
        var compilation = CSharpCompilation.Create(
            assemblyName: "Macaron.InterfaceDelegation.Tests",
            syntaxTrees: [syntaxTree],
            references: references,
            options: new CSharpCompilationOptions(
                outputKind: OutputKind.DynamicallyLinkedLibrary,
                nullableContextOptions: NullableContextOptions.Enable
            )
        );

        var generator = new InterfaceDelegationGenerator();
        GeneratorDriver driver = CSharpGeneratorDriver.Create(generator);
        driver = driver.RunGeneratorsAndUpdateCompilation(
            compilation,
            out var outputCompilation,
            out _
        );
        var result = driver.GetRunResult().Results.Single();
        var allDiagnostics = outputCompilation
            .GetDiagnostics()
            .Concat(result.Diagnostics)
            .ToImmutableArray();

        return new GeneratorTestResult(allDiagnostics, result.GeneratedSources, outputCompilation);
    }
}
