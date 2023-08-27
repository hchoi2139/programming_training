use std::fmt::Write;
use std::{io, collections::HashMap};

const MAX: usize = 200_000;

fn find_parent(parent: &mut [usize], i: usize) -> usize {
    if parent[i] == i { return i; }
    parent[i] = find_parent(parent, parent[i]);
    parent[i]
}

fn make_union(
    parent: &mut [usize], 
    n_friends: &mut [usize], 
    i: usize, 
    j: usize)
{
    let pi = find_parent(parent, i);
    let pj = find_parent(parent, j);
    
    // Exclude the case pi == pj as it means i and j are already friends.

    if pi < pj {
        parent[pj] = pi;
        n_friends[pi] += n_friends[pj];
    } 
    if pj < pi {
        parent[pi] = pj;
        n_friends[pj] += n_friends[pi];
    }
}

fn main() {
    let (mut input, mut output) = (String::new(), String::new());

    io::stdin().read_line(&mut input).expect("Fail: reading t");
    let t: usize = input.trim().parse().expect("Fail: invalid t");
    input.clear();

    let mut parent = [0; MAX + 1];
    let mut n_friends = [1; MAX + 1];
    let mut id_to_pin: HashMap<String, usize> = HashMap::new();
    let mut cnt;

    for _ in 0..t {
        io::stdin().read_line(&mut input).expect("Fail: reading f");
        let f: usize = input.trim().parse().expect("Fail: invalid f");
        input.clear();

        id_to_pin.clear();
        cnt = 0;

        for i in 1..=(2 * f) {
            parent[i] = i;
            n_friends[i] = 1;
        }

        for _ in 0..f {
            io::stdin().read_line(&mut input).expect("Fail: reading friend relationship");
            let ids: Vec<&str> = input.split_whitespace().collect();

            // Define (fst_pin, snd_pin) be corresponding pin no. for input id
            // pair, respectively.
            // If the id is not in the id_to_pin thus was added at the first 
            // time, insert the (id, pin) to the id_to_pin, where pin is the
            // no. of people previously viewed.

            let fst_pin = match id_to_pin.get(ids[0]) {
                Some(pin) => *pin,
                None => {
                    cnt += 1;
                    id_to_pin.insert(ids[0].to_string(), cnt);
                    cnt
                }        
            };

            let snd_pin = match id_to_pin.get(ids[1]) {
                Some(pin) => *pin,
                None => {
                    cnt += 1;
                    id_to_pin.insert(ids[1].to_string(), cnt);
                    cnt
                }
            };

            make_union(&mut parent, &mut n_friends, fst_pin, snd_pin);
            let root = find_parent(&mut parent, fst_pin);
            writeln!(output, "{}", n_friends[root]).unwrap();

            input.clear();
        }
    }

    println!("{output}");
}