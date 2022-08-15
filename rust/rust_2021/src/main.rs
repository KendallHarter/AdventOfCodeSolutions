// Until day dispatch is done
#![allow(dead_code)]

fn day1() {
    let numbers: Vec<i32> = 
        std::io::stdin()
        .lines()
        .map(|x| x.expect("valid line").parse::<i32>().expect("Non-number encountered"))
        .collect();

    let number_pairs = numbers.iter().zip(numbers[1..].iter());
    let window_sums: Vec<i32> = 
        numbers.iter().zip(numbers[1..].iter()).zip(numbers[2..].iter())
        .map(|((x, y), z)| x + y + z)
        .collect();
    let window_pairs = window_sums.iter().zip(window_sums[1..].iter());

    let answer1 =
        number_pairs
        .fold(0, |acc, (num1, num2)| acc + (num2 > num1) as i32);

    let answer2 =
        window_pairs
        .fold(0, |acc, (num1, num2)| acc + (num2 > num1) as i32);
    
    println!("Part 1: {}", answer1);
    println!("Part 2: {}", answer2);
}

enum Direction {
    Up(i32),
    Down(i32),
    Forward(i32)
}

fn to_dir(input: &str) -> Direction {
    let mut iter = input.split_whitespace();
    let dir = iter.next();
    let value: i32 = iter.next().expect("No number").parse().expect("Non-number");
    match dir {
        Some("up") => Direction::Up(value),
        Some("down") => Direction::Down(value),
        Some("forward") => Direction::Forward(value),
        Some(other) => panic!("Invalid direction {}", other),
        None => panic!("No direction given")
    }
}

fn day2() {
    let directions: Vec<Direction> =
        std::io::stdin()
        .lines()
        .map(|x| to_dir(&x.expect("stdin reading failed")))
        .collect();

    {
        let mut depth = 0;
        let mut horizontal = 0;
        for dir in &directions {
            match dir {
                Direction::Up(val) => depth -= val,
                Direction::Down(val) => depth += val,
                Direction::Forward(val) => horizontal += val
            }
        }
        println!("Part 1: {}", depth * horizontal);
    }

    {
        let mut depth = 0;
        let mut horizontal = 0;
        let mut aim = 0;
        for dir in &directions {
            match dir {
                Direction::Up(val) => aim -= val,
                Direction::Down(val) => aim += val,
                Direction::Forward(val) => {
                    horizontal += val;
                    depth += val * aim;
                }
            }
        }
        println!("Part 2: {}", depth * horizontal);
    }
}

fn day3() {
    fn part2calc(mut input: Vec<i32>, num_bits: i32, swap_filter: bool) -> i32 {
        for i in (0..num_bits).rev() {
            let one_count = input.iter().filter(|&x| x & (1 << i) != 0).count();
            if (one_count * 2 >= input.len()) ^ swap_filter {
                input.retain(|&x| x & (1 << i) != 0)
            }
            else {
                input.retain(|&x| x & (1 << i) == 0)
            }
            if input.len() == 1 {
                break;
            }
        }
        return input[0];
    }

    let numbers_raw: Vec<String> = 
        std::io::stdin()
        .lines()
        .map(|val| val.unwrap())
        .collect();

    let num_bits = numbers_raw[0].len();
    let numbers: Vec<i32> = 
        numbers_raw.iter().map(|val| i32::from_str_radix(val, 2).expect("Non-number")).collect();

    // This can probably be a filter/length thing instead
    let mut counts = vec![0; num_bits];
    for i in 0..num_bits {
        for num in &numbers {
            if num & (1 << i) != 0 {
                counts[i] += 1;
            }
        }
    }
    let mut gamma = 0;
    let mut epsilon = 0;
    for i in 0..num_bits {
        if counts[i] * 2 > numbers.len() {
            gamma += 1 << i;
        }
        else {
            epsilon += 1 << i;
        }
    }

    println!("Part 1: {}", gamma * epsilon);

    let oxygen = part2calc(numbers.clone(), num_bits as i32, false);
    let co2 = part2calc(numbers, num_bits as i32, true);
    println!("Part 2: {}", oxygen * co2);
}

struct BingoBoard {
    board: [i32; 25],
    marked: [bool; 25],
}

fn day4() {

}

fn main() {
    let funcs = [day1, day2, day3, day4];
    let mut args = std::env::args();
    let usage = format!("Usage: {} day_to_run", args.next().unwrap());
    let day = args.next().expect(&usage).parse::<i32>().expect(&usage);
    if day < 0 || day > funcs.len() as i32 {
        eprintln!("{}", usage);
    }
    funcs[day as usize]();
}