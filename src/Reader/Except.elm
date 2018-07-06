module Reader.Except
    exposing
        ( Except
        , return
        , fail
        , map
        , andMap
        , andThen
        , join
        )

import Reader exposing (Reader)
import Result exposing (Result(..))


type alias Except env err val =
    Reader env (Result err val)


return : a -> Except env err a
return =
    Reader.reader << Ok


fail : err -> Except env err a
fail =
    Reader.reader << Err


map : (a -> b) -> Except env err a -> Except env err b
map =
    Reader.map << Result.map


andMap : Except env err a -> Except env err (a -> b) -> Except env err b
andMap v f =
    let
        go k =
            v |> Reader.andThen (unpack fail (return << k))
    in
        f |> Reader.andThen (unpack fail go)


andThen : (a -> Except env err a) -> Except env err a -> Except env err a
andThen f x =
    x |> Reader.andThen (unpack fail f)


join : Except env err (Except env err a) -> Except env err a
join x =
    x |> Reader.andThen (unpack fail identity)



-- HELPERS


unpack : (e -> b) -> (a -> b) -> Result e a -> b
unpack ferr fval result =
    case result of
        Err err ->
            ferr err

        Ok val ->
            fval val


resultAndMap : Result e a -> Result e (a -> b) -> Result e b
resultAndMap rx rf =
    case ( rx, rf ) of
        ( _, Err err ) ->
            Err err

        ( x, Ok f ) ->
            Result.map f x
