use std::fs;

// pub fn get_input(day: i32, sample: bool) -> String {
//     let x = format!(
//         "../inputs/{}.{}",
//         day,
//         if sample { "sample" } else { "txt" }
//     );
//     fs::read_to_string(x).unwrap()
// }

#[cfg(debug_assertions)]
pub fn get_input(day: i32) -> String {
    fs::read_to_string(format!("../inputs/{}.sample", day,)).unwrap()
}

#[cfg(not(debug_assertions))]
pub fn get_input(day: i32) -> String {
    fs::read_to_string(format!("../inputs/{}.txt", day,)).unwrap()
}
