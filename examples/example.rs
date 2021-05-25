use generate;

fn main() {
    let data = vec![1, 2, 3, 4];
    let iter = generate::generate![1, 2, ..data];
    
    let data: Vec<i32> = iter.collect();
    println!("{:#?}", data);
}
