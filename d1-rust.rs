fn solve(xs: &Vec<i32> ) -> i32 {

    let n = xs.len();
    let mut ans : i32 = 0;
    for i in 0..n-2 {
        if xs[i] < xs[i+1]  {
            ans = ans  + 1;
        }
    }
    ans
}

fn main(){
    use std::env::args;
    use std::fs::read_to_string;

    let input_file =
        args()
        .skip(1)
        .next()
        .expect("Input file not provided"); 
    let input = read_to_string(&input_file).unwrap();
    let xs = 
        input
        .split('\n')
        .filter(|s| !s.is_empty())
        .map(|x| x.parse::<i32>().unwrap())
        .collect::<Vec<_>>();

    let n = xs.len();
    println!("Input file: {}", input_file);
    println!("{}" , solve(&xs));
}
