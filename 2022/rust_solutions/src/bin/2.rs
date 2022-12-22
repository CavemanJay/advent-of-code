use std::collections::HashMap;

use advent_of_code::get_input;

fn main() {
    let input = get_input(2);
    let round_table_v1 = HashMap::from([
        (("A", "X"), 3),
        (("A", "Y"), 6),
        (("A", "Z"), 0),
        (("B", "X"), 0),
        (("B", "Y"), 3),
        (("B", "Z"), 6),
        (("C", "X"), 6),
        (("C", "Y"), 0),
        (("C", "Z"), 3),
    ]);
    let hand_v1 = HashMap::from([("X", 1), ("Y", 2), ("Z", 3)]);

    let round_table_v2 = HashMap::from([
        (("A", "X"), 3),
        (("A", "Y"), 1),
        (("A", "Z"), 2),
        (("B", "X"), 1),
        (("B", "Y"), 2),
        (("B", "Z"), 3),
        (("C", "X"), 2),
        (("C", "Y"), 3),
        (("C", "Z"), 1),
    ]);
    let hand_v2 = HashMap::from([("X", 0), ("Y", 3), ("Z", 6)]);
    solve(&input, &hand_v1, &round_table_v1);
    solve(&input, &hand_v2, &round_table_v2);
}

fn solve<'a>(
    input: &'a str,
    hand: &HashMap<&'a str, i32>,
    round_table: &HashMap<(&'a str, &'a str), i32>,
) {
    let round_scores: i32 = input
        .lines()
        .map(|round| {
            let (opp, me) = round.split_once(' ').unwrap();
            hand[me] + round_table[&(opp, me)]
        })
        .sum();
    println!("{}", round_scores)
}
