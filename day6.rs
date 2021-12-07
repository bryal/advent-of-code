// static INPUT: &'static str = "1,1,1,1,2,1,1,4,1,4,3,1,1,1,1,1,1,1,1,4,1,3,1,1,1,5,1,3,1,4,1,2,1,1,5,1,1,1,1,1,1,1,1,1,1,3,4,1,5,1,1,1,1,1,1,1,1,1,3,1,4,1,1,1,1,3,5,1,1,2,1,1,1,1,4,4,1,1,1,4,1,1,4,2,4,4,5,1,1,1,1,2,3,1,1,4,1,5,1,1,1,3,1,1,1,1,5,5,1,2,2,2,2,1,1,2,1,1,1,1,1,3,1,1,1,2,3,1,5,1,1,1,2,2,1,1,1,1,1,3,2,1,1,1,4,3,1,1,4,1,5,4,1,4,1,1,1,1,1,1,1,1,1,1,2,2,4,5,1,1,1,1,5,4,1,3,1,1,1,1,4,3,3,3,1,2,3,1,1,1,1,1,1,1,1,2,1,1,1,5,1,3,1,4,3,1,3,1,5,1,1,1,1,3,1,5,1,2,4,1,1,4,1,4,4,2,1,2,1,3,3,1,4,4,1,1,3,4,1,1,1,2,5,2,5,1,1,1,4,1,1,1,1,1,1,3,1,5,1,2,1,1,1,1,1,4,4,1,1,1,5,1,1,5,1,2,1,5,1,1,1,1,1,1,1,1,1,1,1,1,3,2,4,1,1,2,1,1,3,2";
//static INPUT: &'static str = "3,4,3,1,2";
static INPUT: &'static str = "0";


fn main() {
    let mut n_per_lifespan = [0u64; 9];
    for lifespan in INPUT.split(',').map(|s| s.parse::<usize>().unwrap()) {
        n_per_lifespan[lifespan] += 1;
    }
    println!("40: {}", alive_after_n_generations(n_per_lifespan, 40));
    println!("80: {}", alive_after_n_generations(n_per_lifespan.clone(), 80));
    println!("100: {}", alive_after_n_generations(n_per_lifespan, 100));
    println!("150: {}", alive_after_n_generations(n_per_lifespan, 150));
    println!("256: {}", alive_after_n_generations(n_per_lifespan, 256));
}

fn alive_after_n_generations(mut n_per_lifespan: [u64; 9], generations: usize) -> u64 {
    for _ in 0..generations {
        let n_producing = n_per_lifespan[0];
        n_per_lifespan.rotate_left(1);
        n_per_lifespan[6] += n_producing;
        n_per_lifespan[8] = n_producing;
    }
    return n_per_lifespan.iter().sum()
}
