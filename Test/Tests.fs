module Tests

open System
open Xunit
open FsCheck
open FsCheck.Xunit

open Ast
open TestUtil

/// 1~10000 ���� ������ (key * value) �� ����
let keyValueGen: Gen<int * int> =
    Gen.zip (Gen.choose(1,10000)) (Gen.choose(1,10000))

/// 1~10000 ���� ������ (key * value) �� ����Ʈ ����
let keyValueListGen: Gen<(int * int) list> =
    // ���� ������ �ʴ� ��찡 �־ ������ ���� �ϳ��� ���� ����
    Gen.map2 (fun kv kvs -> kv :: kvs) keyValueGen (Gen.listOf keyValueGen) 

/// 1~10000 ���� ������ int ����
let numGen : Gen<int> =
    Gen.choose(1, 10000)


/// key value �� ����Ʈ ����
type IntKeyValueList =
    static member __ () =
        keyValueListGen
        |> Arb.fromGen

/// int ���� ������ ����Ʈ ����
type IntList =
    static member __ () =
        // ���� ������ �ʴ� ��찡 �־ ������ ���� �ϳ��� ���� ����
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


