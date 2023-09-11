module Tests

open System
open Xunit
open FsCheck
open FsCheck.Xunit

open Ast
open TestUtil

/// 1~10000 범위 사이의 (key * value) 쌍 포맷
let keyValueGen: Gen<int * int> =
    Gen.zip (Gen.choose(1,10000)) (Gen.choose(1,10000))

/// 1~10000 범위 사이의 (key * value) 쌍 리스트 포맷
let keyValueListGen: Gen<(int * int) list> =
    // 값이 생기지 않는 경우가 있어서 무조건 값이 하나는 들어가게 구성
    Gen.map2 (fun kv kvs -> kv :: kvs) keyValueGen (Gen.listOf keyValueGen) 

/// 1~10000 범위 사이의 int 포맷
let numGen : Gen<int> =
    Gen.choose(1, 10000)


/// key value 쌍 리스트 생성
type IntKeyValueList =
    static member __ () =
        keyValueListGen
        |> Arb.fromGen

/// int 값을 가지는 리스트 생성
type IntList =
    static member __ () =
        // 값이 생기지 않는 경우가 있어서 무조건 값이 하나는 들어가게 구성
        Gen.map2 (fun kv kvs -> kv :: kvs) numGen (Gen.listOf numGen)
        |> Arb.fromGen

/////////////////////////////////////////////////////////////////////

[<Property(Arbitrary = [| typeof<IntKeyValueList> |], Verbose=true, MaxTest=1000)>]
let ``test ofList/toList (add)`` (ls: (int*int) list) =
    let li = ls |> dedupTupleList
    let env = Env.ofList li
    let envList  = env.toList ()
    
    Assert.Equal<int*int>(li, envList)


[<Property(Arbitrary = [| typeof<IntKeyValueList>; typeof<IntList> |], Verbose=true, MaxTest=1000)>]
let ``test delete`` (ls: (int*int) list) (filterLs: int list) =
    let li = ls |> dedupTupleList
    let filterKeyList = filterLs |> dedupList

    // Expected
    let expect = filterList li filterKeyList 

    // Result
    let env = Env.ofList li
    filterKeyList |> List.iter (fun k -> env.del(k) |> ignore )
    let result  = env.toList()

    Assert.Equal<int*int>(expect, result)


