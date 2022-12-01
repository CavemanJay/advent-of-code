using System.Reflection;

namespace csharp;

public static class Utils
{
    public static TOutput Let<TInput, TOutput>(this TInput val,
        Func<TInput, TOutput> next) => next(val);

    public static string GetDayInput(int day)
    {
        var resource = Assembly.GetExecutingAssembly()
                               .GetManifestResourceNames()
                               .First(x => x.Contains($"day {day} input.txt"));
        using var stream = Assembly.GetExecutingAssembly()
                                   .GetManifestResourceStream(resource);
        using var reader = new StreamReader(stream);
        return reader.ReadToEnd();
    }
}