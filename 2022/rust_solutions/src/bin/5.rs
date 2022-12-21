use advent_of_code::get_input;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till1},
    character::complete::{self, alpha1, char, newline},
    combinator::map,
    multi::separated_list1,
    sequence::{delimited, terminated, tuple},
    IResult,
};

#[derive(Debug)]
struct Move {
    amount: u8,
    from: u8,
    to: u8,
}

fn parse_cargo(input: &str) -> IResult<&str, Vec<Vec<Option<char>>>> {
    let no_crate = map(tag("   "), |_| None);
    let yes_crate = delimited(char('['), alpha1, char(']'));
    let yes_crate = map(yes_crate, |s: &str| Some(s.chars().next().unwrap()));
    let maybe_crate = alt((no_crate, yes_crate));
    let line = separated_list1(char(' '), maybe_crate);
    let lines = separated_list1(char('\n'), line);

    let (input, res) = terminated(lines, newline)(input)?;
    let (input, _garbage) = terminated(take_till1(|c: char| c == '\n'), tag("\n\n"))(input)?;

    Ok((input, res))
}

fn parse_moves(input: &str) -> IResult<&str, Vec<Move>> {
    let num = complete::u8;
    let cmd = map(
        tuple((tag("move "), num, tag(" from "), num, tag(" to "), num)),
        |(_, amount, _, from, _, to)| Move { amount, from, to },
    );
    let mut moves = separated_list1(newline, cmd);

    let (input, res) = moves(input)?;

    Ok((input, res))
}

fn main() {
    let input = get_input(5);
    let (input, cargo) = parse_cargo(&input).unwrap();
    let (input, moves) = parse_moves(&input).unwrap();
    assert_eq!(input, "");
}
