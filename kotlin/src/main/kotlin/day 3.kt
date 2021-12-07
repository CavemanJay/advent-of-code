private val sampleData =
    listOf(
        "00100",
        "11110",
        "10110",
        "10111",
        "10101",
        "01111",
        "00111",
        "11100",
        "10000",
        "11001",
        "00010",
        "01010"
    )
        .map(String::toCharArray)
        .map(CharArray::asList)

private fun readBin(binStr: String) = binStr.toInt(2)

private enum class Commonality {
    Least,
    Most
}

private fun commonality(c: Commonality, digits: List<Char>): Int {
    val groups = digits.groupBy { it -> it == '0' }
    val zeroCount = groups[true]!!.size
    val oneCount = groups[false]!!.size

    if (zeroCount == oneCount) {
        return when (c) {
            Commonality.Least -> 0
            Commonality.Most -> 1
        }
    }

    return when (zeroCount < oneCount) {
        true -> when (c) {
            Commonality.Least -> 0
            Commonality.Most -> 1
        }
        false -> when (c) {
            Commonality.Least -> 1
            Commonality.Most -> 0
        }
    }
}

private fun bitFilter(index: Int, common: Commonality, bins: List<List<Char>>): String {
    if (bins.size == 1) return bins.first().joinToString("")

    val bitVal = bins
        .transpose()[index]
        .let { commonality(common, it) }
        .toString()[0]
    val matching = bins.filter { it[index] == bitVal }

    return bitFilter(index + 1, common, matching)
}

/**
 * Based on: [Stack Overflow Answer](https://stackoverflow.com/a/70230823)
 */
private fun <T> List<List<T>>.transpose(): List<List<T>> {
    val cols = this[0].size
    val rows = this.size
    return List(cols) { j ->
        List(rows) { i ->
            this[i][j]
        }
    }
}

fun main() {
    val input = Utils
        .getInputLines(3)
        .map(String::toCharArray)
        .map(CharArray::asList)

    val partOne = Commonality.values()
        .map { common ->
            input
                .transpose()
                .map { digits ->
                    commonality(common, digits)
                }
                .joinToString("")
        }
        .map(::readBin)
        .fold(1) { acc, i -> acc * i }

    val partTwo =
        Commonality.values()
            .map { common ->
                bitFilter(0, common, input)
            }
            .map(::readBin)
            .fold(1) { acc, i -> acc * i }
    println(partOne)
    println(partTwo)
}

