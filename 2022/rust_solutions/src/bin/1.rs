use advent_of_code::get_input;

fn main() {
    let input = get_input(1);
    let elves: Vec<_> = input
        .split("\n\n")
        .map(|vals| {
            vals.split("\n")
                .map(|num| num.parse().unwrap())
                .collect::<Vec<i32>>()
        })
        .collect();

    let mut sums: Vec<i32> = elves.iter().map(|pack| pack.iter().sum()).collect();
    let max = &sums.iter().max().unwrap();
    println!("{}", max);

    sums.sort();
    sums.reverse();

    println!("{}", &sums.iter().take(3).sum::<i32>());
}
