import java.net.URL

class Utils {
    companion object {
        fun getDayInput(day: Int): URL {
            val fileName = "day $day input.txt"
            return object {}.javaClass.getResource(fileName)
        }
    }
}