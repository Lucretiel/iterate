fn main() {
    let data = vec![1, 2, 3, 4];
    let data = &data;
    let iter = data.iter();

    let iter = iterate::iterate![&1, &2, ..iter, &3, ..data.iter(), &4];

    let data: Vec<&i32> = iter.collect();
    println!("{:#?}", data);

    let range = 0..5;
    let vec = vec![4, 1, 2, 3];

    let iterator = iterate::iterate![
        1,
        ..range,
        ..vec.into_iter(),
        ..if false { return } else { 0..5 },
        10,
        if false { return } else { 10 },
    ];

    let result: Vec<i32> = iterator.collect();
    println!("{:#?}", result);

    let vec = vec![4, 1, 2, 3];
    let iter = vec.iter().copied();
    let iterator = iterate::iterate![1, ..0..5, ..iter, 10,];
    println!("{:#?}", iterator.size_hint());
}
