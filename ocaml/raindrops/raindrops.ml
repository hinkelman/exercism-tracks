open
let raindrop number =
    let pl_lst = 
        (List.map2 
        (fun fac str -> if (number mod fac) = 0 then str else "") 
        [3; 5; 7] 
        ["Pling"; "Plang"; "Plong"]) in
    let pl_str = String.concat "" pl_lst in
    if pl_str = "" then string_of_int number else pl_str
