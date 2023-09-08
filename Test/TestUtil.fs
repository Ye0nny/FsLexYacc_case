module TestUtil


let dedupTupleList li =
    let result = 
        li 
        |> List.groupBy fst 
        |> List.map (fun (key, group) -> List.head group)
    result

    
let dedupList li =
    if List.isEmpty li then
        li
    else
        li |> List.distinct


let filterList li filterKeyList = 
    li 
    |> List.filter (
        fun (key,_) -> 
            List.contains key filterKeyList 
            |> not
        )

