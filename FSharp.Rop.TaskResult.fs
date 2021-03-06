module FSharp.Rop.TaskResult

open System.Threading.Tasks
open FSharp.Control.Tasks

[<RequireQualifiedAccess>]
module TaskResult =
    let bind2 f (x:Task<Result<'a,_>>) (y:Task<Result<'b,_>>) =
        task {
            let! resX = x
            let! resY = y
            match resX, resY with
            | Ok xR, Ok yR -> return! f xR yR
            | Error e, _ | _, Error e -> return Error e
        }

    let ofTaskAndResult (fn:'a -> Task<'b>) (res:Result<'a, _>) =
        task {
            match res with
            | Ok a ->
                let! r = fn a
                return Ok r
            | Error e -> return Error e
        }
    
    let ofResult (res:Result<_,_>) = task { return res }
    
    let ofTask (t:Task<'a>) = 
        task {
            let! v = t
            return (Ok v)
        }
    
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
    
    let mapTask (f:'a -> Task<'b>) (result:Task<Result<'a,_>>) =
        task {
            match! result with
            | Ok s ->
                let! r = f s
                return r |> Ok
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

    let defaultWith (fn:'b -> Task<'a>) (result:Task<Result<'a,'b>>) =
        task {
            match! result with
            | Ok v -> return v
            | Error e -> return! e |> fn
        }

module Operators =
    let (>>=) x f = TaskResult.bind f x
    let (<!>) = TaskResult.map
    let (|>>) x f = TaskResult.map f x
    let (<*>) = TaskResult.apply
