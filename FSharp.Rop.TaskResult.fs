module FSharp.Rop.TaskResult

open System.Threading.Tasks
open FSharp.Control.Tasks

[<RequireQualifiedAccess>]
module TaskResult =
    
    let ofTaskAndResult (fn:'a -> Task<'b>) (res:Result<'a, _>) =
        task {
            match res with
            | Ok a ->
                let! r = fn a
                return Ok r
            | Error e -> return Error e
        }
    
    let ofResult (res:Result<_,_>) = task { return res }
    
    let bind (f:'a -> Task<Result<'b,_>>) (result:Task<Result<'a,_>>) = 
        task {
            match! result with
            | Ok s -> return! (f s)
            | Error f -> return Error f
        }
    
    let map (f:'a -> 'b) (result:Task<Result<'a,_>>) =
        task {
            match! result with
            | Ok s -> return s |> f |> Ok
            | Error e -> return Error e
        }
    
    let mapError (f:'b -> 'c) (result:Task<Result<'a,'b>>) = 
        task {
            match! result with
            | Ok x -> return Ok x
            | Error e -> return Error (f e)
        }
    
    let apply (resultF:Task<Result<'a -> 'b, 'c>>) (result:Task<Result<'a,'c>>) =
        task {
            match! resultF with
            | Ok f -> return! map f result
            | Error e -> return (Error e)
        }

module Operators =
    let (>>=) result f = TaskResult.bind f result
    let (<!>) result f = TaskResult.bind (f >> Ok >> Task.FromResult) result
    let (<*>) = TaskResult.apply
