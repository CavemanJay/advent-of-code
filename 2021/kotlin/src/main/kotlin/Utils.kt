import java.net.URL
import java.util.*
import kotlin.collections.RandomAccess

class Utils {
    companion object {
        fun getDayInput(day: Int): URL {
            val fileName = "day $day input.txt"
            return object {}.javaClass.getResource(fileName)
        }

        fun getInputLines(day: Int): List<String> {
            return getDayInput(day)
                .readText()
                .split(Regex("""(\r)?\n"""))
        }
    }
}

/**
 * Based on: [Stack Overflow Answer](https://stackoverflow.com/a/70230823)
 */
fun <T> List<List<T>>.transpose(): List<List<T>> {
    val cols = this[0].size
    val rows = this.size
    return List(cols) { j ->
        List(rows) { i ->
            this[i][j]
        }
    }
}
