import java.util.*

private val sampleData = """
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7 
""".trimIndent()

private enum class MarkStatus {
    Marked,
    Unmarked
}

private data class MarkableInt(
    var status: MarkStatus,
    val value: Int
) {
    fun isMarked() = this.status == MarkStatus.Marked
}

private data class CompletedBoard(
    val board: Board,
    val finalValue: Int
)

private class Board(private val cells: MutableList<MutableList<MarkableInt>>) {
    companion object {
        fun parseBoard(str: String): Board {
            val rows = str.split("\n").map(String::strip)
            val rowNums = rows
                .map { row ->
                    row
                        .split(" ")
                        .map(String::toInt)
                        .map { MarkableInt(MarkStatus.Unmarked, it) }
                        .toMutableList()
                }
                .toMutableList()

            return Board(rowNums)
        }
    }

    fun hasWon(): Boolean {
        fun rowWin(b: Board): Boolean {
            return b.cells.any { row ->
                row.all(MarkableInt::isMarked)
            }
        }

        val colWin: Boolean by lazy {
            this.cells.transpose().map { it.toMutableList() }.toMutableList().let(::Board).let(::rowWin)
        }

        return rowWin(this) || colWin
    }

    fun markValue(value: Int) {
        this.cells.flatten().filter { it.value == value }.forEach { it.status = MarkStatus.Marked }
    }

    fun boardSum() = cells
        .flatten()
        .filter { it.status == MarkStatus.Unmarked }
        .map(MarkableInt::value)
        .sum()
}

private fun order(input: String): List<Int> {
    return input
        .split("\n")
        .first()
        .split(",")
        .map(String::toInt)
}

private fun partOne(values: Iterable<Int>, boards: Iterable<Board>): CompletedBoard {
    var finishedBoard: CompletedBoard? = null
    values.forEach { value ->
        if (finishedBoard != null) {
            return@forEach
        }

        boards.forEach { board ->
            board.markValue(value)
        }

        val completed = boards.filter(Board::hasWon)
        if (completed.isNotEmpty()) {
            finishedBoard = CompletedBoard(completed.first(), value)
        }
    }
    return finishedBoard!!
}

private fun partTwo(values: Iterable<Int>, boards: Iterable<Board>): Stack<CompletedBoard> {
    val finishedBoards = Stack<CompletedBoard>()
    val done = mutableListOf<Board>()
    values.forEach { value ->
        boards.forEach { board ->
            if (done.contains(board))
                return@forEach

            board.markValue(value)
            if (board.hasWon()) {
                finishedBoards.add(CompletedBoard(board, value))
                done.add(board)
            }
        }
    }

    return finishedBoards
}

fun main() {
    val _input = Utils.getInputLines(4)
    val input = _input
//    val input = sampleData.split("\n")

    val boardsText = input
        .slice(2 until input.size)
        .joinToString("\n")
        .replace("  ", " ")

    val boards = boardsText
        .split("\n\n")
        .map(Board::parseBoard)


    val _order = order(input.joinToString("\n"))
    val _partOne = partOne(_order, boards).let { it.board.boardSum() * it.finalValue }
    val _partTwo = partTwo(_order, boards).pop().let { it.board.boardSum() * it.finalValue }
    println(_partOne)
    println(_partTwo)
}