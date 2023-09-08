module Tests

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open FsCheck.FSharp

open Ast
open TestUtil

type IntList10000 =
    static member __ () =
        Gen.choose (1, 1000)
        |> Arb.fromGen


[<Property(Arbitrary = [| typeof<IntList10000> |], Verbose=true, MaxTest=1000)>]
let ``test ofList/toList (add)`` (ls: (int*int) list) =
    let li = ls |> dedupTupleList
    let env = Env.ofList li
    let envList  = env.toList ()
    
    Assert.Equal<int*int>(li, envList)


[<Property(Arbitrary = [| typeof<IntList10000> |], Verbose=true, MaxTest=1000)>]
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


