private val sampleData = listOf("forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2")

private enum class MovementType {
    Forward,
    Down,
    Up
}

private data class Position(
    val horizontal: Int,
    val depth: Int,
    val aim: Int?
)

private data class Movement(
    val type: MovementType,
    val value: Int
) {
    companion object {
        fun parse(line: String): Movement {
            val parts = line.split(" ")
            return when (parts[0]) {
                "forward" -> Movement(MovementType.Forward, parts[1].toInt())
                "down" -> Movement(MovementType.Down, parts[1].toInt())
                "up" -> Movement(MovementType.Up, parts[1].toInt())
                else -> throw NotImplementedError()
            }
        }
    }

    fun depth(): Int {
        return when (this.type) {
            MovementType.Up -> -1 * this.value
            MovementType.Down -> this.value
            MovementType.Forward -> 0
        }
    }

    fun dist(): Int {
        return when (this.type) {
            MovementType.Forward -> this.value
            else -> 0
        }
    }
}

private fun partOne(moves: List<Movement>): Position {
    val horizontal = moves.map(Movement::dist).sum()
    val depth = moves.map(Movement::depth).sum()

    return Position(horizontal, depth, null)
}

private fun calc(moves: List<Movement>, pos: Position): Position {
    if (moves.isEmpty()) return pos

    val move = moves.first()
    return calc(moves.slice(1 until moves.size), nextPos(move, pos))
}

private fun nextPos(move: Movement, pos: Position): Position {
    return when (move.type) {
        MovementType.Forward -> Position(
            pos.horizontal + move.value,
            pos.depth + pos.aim!! * move.value,
            pos.aim
        )
        MovementType.Up -> Position(

            pos.horizontal,
            pos.depth,
            pos.aim!! - move.value
        )
        MovementType.Down -> Position(
            pos.horizontal,
            pos.depth,
            pos.aim!! + move.value
        )
    }
}

private fun partTwo(moves: List<Movement>): Position = calc(moves, Position(0, 0, 0))


fun main() {
    val inputData = Utils.getInputLines(2)

    val _partOne = inputData
        .map(Movement::parse)
        .let(::partOne)
        .let { it.horizontal * it.depth }

    val _partTwo = inputData
        .map(Movement::parse)
        .let(::partTwo)
        .let { it.horizontal * it.depth }

    println(_partOne)
    println(_partTwo)
}