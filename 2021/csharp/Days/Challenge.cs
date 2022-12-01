namespace csharp.Days;

public abstract class Challenge<TInput, TOutput>
{
    protected abstract TInput SampleData { get; }
    protected abstract TOutput PartOne(TInput? data);
    protected abstract TOutput PartTwo(TInput? data);

    public (TOutput partOne, TOutput partTwo)
        Answers(TInput? data) => (
        this.PartOne(data ?? this.SampleData),
        this.PartTwo(data ?? this.SampleData));

    public abstract TInput GetInput();

}