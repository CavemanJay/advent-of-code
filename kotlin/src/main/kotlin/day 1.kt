private val sampleData = listOf(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

private enum class Change {
    Increase,
    Decrease
}

private typealias Window = List<Int>

private fun transform(vals: List<Int>): List<Pair<Int, Int>> {
    return when (vals.size) {
        0, 1 -> emptyList()
        else -> listOf(Pair(vals[0], vals[1])) + transform(vals.slice(1 until vals.size))
    }
}

private fun getChange(p: Pair<Int, Int>): Change {
    val x = p.first
    val y = p.second
    return if (y > x) Change.Increase else Change.Decrease
}

private fun countIncreases(vals: List<Int>): Int {
    return transform(vals).map(::getChange).filter { it == Change.Increase }.size
}

private fun toWindows(vals: List<Int>): List<Window> {
    if (vals.size in 0..2)
        return emptyList()

    val window: Window = listOf(vals[0], vals[1], vals[2])
    return listOf(window) + toWindows(vals.slice(1 until vals.size))
}

fun main() {
    val inputData = Utils
        .getInputLines(1)
        .map(String::toInt)

    val partOne = countIncreases(inputData)
    val partTwo = toWindows(inputData)
        .map(Iterable<Int>::sum)
        .let(::countIncreases)

    println(partOne)
    println(partTwo)
}