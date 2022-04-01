/// Implements a Kahan summation algorithm over the iterator.
pub fn sum(arr: impl IntoIterator<Item = f64>) -> f64 {
    let mut sum = 0.0;
    let mut c = 0.0;
    for x in arr {
        let y = x - c;
        let t = sum + y;
        c = (t-sum) - y;
        sum = t;
    }
    sum
}