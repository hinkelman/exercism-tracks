
let check_non_zero x y z =
    x > 0 && y > 0 && z > 0

let check_max_side x y z = 
    let perimeter = x + y + z in
    let max_side = List.fold_left max x [y; z] in
    perimeter - max_side >= max_side

let check_triangle x y z =
    check_non_zero x y z &&
    check_max_side x y z

let is_equilateral x y z =
    check_triangle x y z && 
    x = y && y = z

let is_isosceles x y z =
    check_triangle x y z &&
    (x = y || x = z || y = z)

let is_scalene x y z =
    check_triangle x y z &&
    x <> y && y <> z