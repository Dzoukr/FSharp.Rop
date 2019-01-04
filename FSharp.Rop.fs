module FSharp.Rop

// basic module extension
module Result =
    let bind2 f x y = 
        match x, y with
        | Ok xR, Ok yR -> f xR yR
        | Error e, _ | _, Error e -> Error e

    let apply resultF result =
        match resultF with
        | Ok f -> Result.map f result
        | Error e -> Error e
    
    let fold results =
        let foldFn item acc =
            match acc, item with
            | Error e, _ | _, Error e -> Error e
            | Ok l, Ok v -> v :: l |> Ok
        List.foldBack foldFn results (Ok [])

    let foldAll results =
        let foldFn item acc =
            match acc, item with
            | Error ae, Error ie -> ie :: ae |> List.rev |> Error
            | Error e, Ok _ -> Error e
            | Ok _, Error ie -> ie |> List.singleton |> Error 
            | Ok l, Ok v -> v :: l |> Ok
        List.foldBack foldFn results (Ok [])

    let isOk = function
        | Ok _ -> true
        | Error _ -> false

    let isError result = result |> isOk |> not

    let toOption = function
        | Ok v -> Some v
        | Error _ -> None

    let ofOption defError = function
        | Some v -> Ok v
        | None -> Error defError

    let withDefault def = function
        | Ok v -> v
        | Error _ -> def
    
    let private getOk = function
        | Ok v -> v
        | Error e -> failwithf "Expected Ok value, but got %A error instead" e
    
    let private getError = function
        | Error e -> e
        | Ok v -> failwithf "Expected Error, but got %A Ok value instead" v

    let partition results =
        let ok, errs = results |> List.partition (isOk)
        ok |> List.map (getOk), errs |> List.map (getError)

    let traverse f list =
        let retn = Result.Ok
        let cons head tail = head :: tail
        let (<*>) = apply

        let init = retn []
        let folder (head:'a) (tail:Result<'b list,'c>) = 
            apply (apply (retn cons) (f head)) tail
        List.foldBack folder list init

    let log (fn:Result<'a,'b> -> unit) result =
        result |> fn
        result

// computation expression
type ResultBuilder() = 
    member __.Zero() = Ok()
    member __.Bind(m, f) = Result.bind f m
    member __.Return(x) = Ok x
    member __.ReturnFrom(x) = x
    member __.Combine (a, b) = Result.bind b a
    member __.Delay f = f
    member __.Run f = f ()
    member __.TryWith (body, handler) =
        try
            body()
        with
        | e -> handler e
    member __.TryFinally (body, compensation) =
        try
            body()
        finally
            compensation()
    member x.Using(d:#System.IDisposable, body) =
        let result = fun () -> body d
        x.TryFinally (result, fun () ->
            match d with
            | null -> ()
            | d -> d.Dispose())
    member x.While (guard, body) =
        if not <| guard () then
            x.Zero()
        else
            Result.bind (fun () -> x.While(guard, body)) (body())
    member x.For(s:seq<_>, body) =
        x.Using(s.GetEnumerator(), fun enum ->
            x.While(enum.MoveNext,
                x.Delay(fun () -> body enum.Current)))

let result = ResultBuilder()

// custom operators
let (>>=) result f = Result.bind f result
let (<!>) result f = Result.map f result
let (<*>) = Result.apply