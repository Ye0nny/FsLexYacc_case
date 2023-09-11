module TestUtil

/// (k * v) list의 중복 제거
let dedupTupleList li =
    let result = 
        li 
        |> List.groupBy fst 
        |> List.map (fun (key, group) -> List.head group)
    result

/// list의 중복 제거
let dedupList li =
    if List.isEmpty li then
        li
    else
        li |> List.distinct

/// (k * v) list에서 key list에 들어있는 값과 일치하는 k는 list에서 제외
let filterList li filterKeyList = 
    li 
    |> List.filter (
        fun (key,_) -> 
            List.contains key filterKeyList 
            |> not
        )

