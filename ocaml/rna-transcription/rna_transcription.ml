type dna = [ `A | `C | `G | `T ]
type rna = [ `A | `C | `G | `U ]

let rna_complement dna =
    match dna with
    | `G -> `C
    | `C -> `G
    | `T -> `A
    | `A -> `U

let to_rna dna_list =
    List.map rna_complement dna_list
