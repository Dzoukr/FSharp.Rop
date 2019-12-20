module FSharp.Rop

open System.Threading.Tasks
open FSharp.Control.Tasks

module TaskResult =
    
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
        let (>>=) result f = bind f result
        let (<!>) result f = bind (f >> Ok >> Task.FromResult) result
        let (<*>) = apply