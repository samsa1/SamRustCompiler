// Heap is stored as :
// 

const NULL_PTR : usize = 18_446_744_073_709_551_615usize;
const U32_MAX : usize = 1 << 32;
const FREE_BIT : u32 = 1;

fn const_zero() -> usize {
    0
}

pub fn init(vec: &mut Vec<u32>) {
    vec[0] = (vec.len() - 4) as u32 | FREE_BIT;
    vec[1] = 0;
    let len = vec.len();
    vec[len - 2] = FREE_BIT;
    vec[len - 1] = 0;
}

pub fn malloc(vec : &mut Vec<u32>, size : usize) -> usize {
    print!("allocating\n");
    if size >= U32_MAX {
        return NULL_PTR;
    }
    let mut size = size as u32;
    size = (size + 7) >> 2; // is even
    let mut pos = 0;
    let mut previous = 0;
    while true {
        print_ptr!(&vec[pos]);
        if vec[pos] & FREE_BIT == 1 {
            let available = vec[pos] - 1; // is even
//            println!("free at {pos} of size {available}");
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
                print_ptr!(&vec[pos]);
                print_ptr!(&vec[pos + size as usize + 2]);
                print!("allocated\n");
                return (pos + 2) << 2;
            }
            previous = pos as u32;
            pos = pos + vec[pos] as usize + 1;
        } else {
            if vec[pos] == 0 {
                1 / const_zero();
            }
            previous = pos as u32;
            pos = pos + vec[pos] as usize;
        }
    };
    0
}


// needs to be finished and tested
pub fn realloc(vec : &mut Vec<u32>, pos : usize, size : usize) -> usize {
    if pos == NULL_PTR {
        malloc(vec, size)
    } else if size == 0 {
        free(vec, pos);
        NULL_PTR
    } else {
        let real_pos = (pos >> 2) - 2;
        let next_pos = real_pos + vec[real_pos] as usize;
        let available = if vec[next_pos] & FREE_BIT == 1 {
            vec[real_pos] + vec[next_pos] - 1
        } else {
            vec[real_pos] - 2
        };
        let new_size = (size + 7) >> 3;
        if available as usize >= size {
            let last_pos = next_pos + ((vec[next_pos] | FREE_BIT) + FREE_BIT) as usize;
            print!("todo\n");
            1 / const_zero()
        } else {
            let new_pos = malloc(vec, size);
            let mut offset = 0;
            while offset < (vec[real_pos] as usize - 2)  {
                vec[new_pos + offset] = vec[(pos >> 2) + offset];
                offset = offset + 1;
            }
            free(vec, pos);
            new_pos << 2
        }

    }
}


pub fn free(vec : &mut Vec<u32>, pos : usize) {
    let mut real_pos = (pos - 2) >> 2;
    if vec[real_pos] as u32 & FREE_BIT == 1 {
        print!("Segfault\n");
        1/const_zero();
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
        vec[real_pos] = (last_pos - real_pos - 2) as u32 | FREE_BIT;
    }
}


pub fn testing() {
    let mut array = vec![
        42, 42, 42, 42, 42,
        42, 42, 42, 42, 42,
        42, 42, 42, 42, 42,
        42, 42, 42, 42, 42,
    ];
    init(&mut array);
    print!("{array:?}\n");
    let id1 = malloc(&mut array, 2);
    print!("{array:?} malloc -> {id1}\n");
    let id2 = malloc(&mut array, 1);
    print!("{array:?} malloc -> {id2}\n");
    let id3 = malloc(&mut array, 3);
    print!("{array:?} malloc -> {id3}\n");
    free(&mut array, id2);
    print!("{array:?} free <- {id2}\n");
    free(&mut array, id3);
    print!("{array:?} free <- {id3}\n");

}