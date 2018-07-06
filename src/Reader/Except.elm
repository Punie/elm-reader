module Reader.Except
    exposing
        ( Except
        , succeed
        , fail
        , map
        , andMap
        , andThen
        , join
        )

{-| Sometimes, you may want to build up a computation using `Reader env value` where
the value is a result that may or may not fail. Elm already provides us with the
`Result err value` that provides this functionality.
However, working with nested ADT can be cumbersome. This module exposes the type
`Except env err val` which is just a type alias for a `Reader env (Result err val)`
and provides some useful functions for mapping over or chaining such nested computations.


# The Except type

@docs Except


# Construction

@docs succeed, fail


# Transformations and chaining

@docs map, andMap, andThen, join

-}

import Reader exposing (Reader)
import Result exposing (Result(..))


{-| The Except type wraps the result from a Reader into a Result.
-}
type alias Except env err val =
    Reader env (Result err val)


{-| Embed a successful value inside a Reader.
-}
succeed : a -> Except env err a
succeed =
    Reader.reader << Ok


{-| Embeds a failure inside a Reader.
-}
fail : err -> Except env err a
fail =
    Reader.reader << Err


{-| Apply a function to the resulting value of the Reader if successful.
-}
map : (a -> b) -> Except env err a -> Except env err b
map =
    Reader.map << Result.map


{-| Apply a function embeded in an Except to a successful value in an Except.
-}
andMap : Except env err a -> Except env err (a -> b) -> Except env err b
andMap v f =
    let
        go k =
            v |> Reader.andThen (unpack fail (succeed << k))
    in
        f |> Reader.andThen (unpack fail go)


{-| Chain Excepts together.
-}
andThen : (a -> Except env err a) -> Except env err a -> Except env err a
andThen f x =
    x |> Reader.andThen (unpack fail f)


{-| Discards one level of Except
-}
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
