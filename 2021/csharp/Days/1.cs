using System.Collections.Immutable;

namespace csharp.Days;

public class Day1 : Challenge<IReadOnlyList<int>, int>
{
    protected override IReadOnlyList<int> SampleData { get; } = new List<int>()
        { 199, 200, 208, 210, 200, 207, 240, 269, 260, 263 };

    protected override int PartOne(IReadOnlyList<int>? data) =>
        CountIncreases(data ?? this.SampleData);

    protected override int PartTwo(IReadOnlyList<int>? data) =>
        Windows(data ?? this.SampleData)
            .Select(x => x.Sum())
            .ToList()
            .Let(CountIncreases);

    public override IReadOnlyList<int> GetInput()
    {
        return Utils.GetDayInput(1)
                    .Split("\n")
                    .Select(int.Parse)
                    .ToImmutableList();
    }


    private enum Change
    {
        Increase,
        Decrease,
    }

    private static Change GetChange((int, int) change)
    {
        var (x, y) = change;
        return y > x ? Change.Increase : Change.Decrease;
    }

    private static int CountIncreases(IReadOnlyList<int> data) =>
        Transform(data).Select(GetChange).Count(x => x == Change.Increase);

    private static IEnumerable<(int, int)> Transform(IReadOnlyList<int> values)
    {
        IEnumerable<(int, int)> TransformRec(IReadOnlyList<int> vals,
            IEnumerable<(int, int)> accumulator)
        {
            return vals.Count switch
            {
                < 2 => accumulator,
                _ => TransformRec(
                    vals.Skip(1).ToList(),
                    new List<(int, int)>(accumulator) { (vals[0], vals[1]) }
                ),
            };
        }

        return TransformRec(values, ArraySegment<(int, int)>.Empty);
    }

    private static List<IEnumerable<int>> Windows(IReadOnlyCollection<int> data)
    {
        return data.Count switch
        {
            < 3 => new List<IEnumerable<int>>(),
            _ => new List<IEnumerable<int>>() { data.Take(3) }.Concat(
                    Windows(data.Skip(1).ToImmutableArray())
                )
                .ToList(),
        };
    }
}