// Heap is stored as :
// 

const NULL_PTR : usize = usize::MAX;
const FREE_BIT : u32 = 1;

fn init(vec: &mut Vec<u32>) {
    vec[0] = (vec.len() - 4) as u32 | FREE_BIT;
    vec[1] = 0;
    let len = vec.len();
    vec[len - 2] = FREE_BIT;
    vec[len - 1] = 0;
}

fn malloc(vec : &mut Vec<u32>, size : usize) -> usize {
    if size > u32::MAX as usize {
        return NULL_PTR
    }
    let mut size = size as u32;
    size = (size + 7) >> 2; // is even
    let mut pos = 0;
    let mut previous = 0;
    loop {
        if vec[pos] & FREE_BIT == 1 {
            let available = vec[pos] - 1; // is even
            println!("free at {pos} of size {available}");
            if available >= size + 2 {
                if available > size + 4 {
                    vec[pos + (size as usize) + 2] = (available - size - 2) | FREE_BIT; // is odd
                    vec[pos + (size as usize) + 3] = pos as u32;
                    vec[pos + (available as usize) + 3] = pos as u32 + size + 2;
                } else {
                    size = available
                }
                vec[pos] = size + 2;  
                vec[pos + 1] = previous;              
                return pos + 2
            }
            previous = pos as u32;
            pos = pos + vec[pos] as usize + 1;
        } else {
            previous = pos as u32;
            pos = pos + vec[pos] as usize;
        }
    }
}


// needs to be finished and tested
fn realloc(vec : &mut Vec<u32>, pos : usize, size : usize) -> usize {
    if pos == NULL_PTR {
        malloc(vec, size)
    } else if size == 0 {
        free(vec, pos);
        NULL_PTR
    } else {
        let real_pos = pos - 2;
        let next_pos = real_pos + vec[real_pos] as usize;
        let available = if vec[next_pos] & FREE_BIT == 1 {
            vec[real_pos] + vec[next_pos] - 1
        } else {
            vec[real_pos] - 2
        };
        let new_size = (size + 7) >> 3;
        if available as usize >= size {
            let last_pos = next_pos + ((vec[next_pos] | FREE_BIT) + FREE_BIT) as usize;
            todo!();
        } else {
            let new_pos = malloc(vec, size);
            for offset in 0..(vec[real_pos] as usize - 2)  {
                vec[new_pos + offset] = vec[pos + offset]
            }
            free(vec, pos);
            new_pos
        }

    }
}


fn free(vec : &mut Vec<u32>, pos : usize) {
    let mut real_pos = pos - 2;
    if vec[real_pos] as u32 & FREE_BIT == 1 {
        panic!("Segfault")
    } else {
        let mut last_pos = real_pos + vec[real_pos] as usize/* = size + 2 */;
        if vec[last_pos] & FREE_BIT == 1 {
            last_pos = last_pos + vec[last_pos] as usize + 1; 
        };
        let previous_pos = vec[real_pos + 1] as usize;
        if vec[previous_pos] & FREE_BIT == 1 {
            real_pos = previous_pos
        }
        vec[last_pos + 1] = real_pos as u32;
        vec[real_pos] = (last_pos - real_pos - 2) as u32 | FREE_BIT
    }
}


fn main() {
    let mut array = vec![42; 20];
    init(&mut array);
    println!("{array:?}");
    let id1 = malloc(&mut array, 2);
    println!("{array:?} malloc -> {id1}");
    let id2 = malloc(&mut array, 1);
    println!("{array:?} malloc -> {id2}");
    let id3 = malloc(&mut array, 3);
    println!("{array:?} malloc -> {id3}");
    free(&mut array, id2);
    println!("{array:?} free <- {id2}");
    free(&mut array, id3);
    println!("{array:?} free <- {id3}");

}