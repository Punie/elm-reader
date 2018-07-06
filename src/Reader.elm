module Reader
    exposing
        ( Reader
        , run
        , reader
        , ask
        , local
        , asks
        , map
        , andMap
        , andThen
        , join
        )

{-| Basically, a `Reader` is just a wrapper for a function type.
It is quite handy in that it solves the problem to manually having to
thread a single configuration parameter to many functions.
It is often viewed as a way to do dependency injection in FP.


# The Reader type

@docs Reader


# Construction

@docs run, reader, ask, asks, local


# Transformations and chaining

@docs map, andMap, andThen, join

-}


{-| `Reader env value` represents a computation waiting for an environment
value to produce its result.
-}
type Reader env value
    = Reader (env -> value)


{-| Run the reader, providing it, at last, its environment.
-}
run : Reader env value -> env -> value
run (Reader f) =
    f


{-| Construct a Reader that will produce the value provided, no matter the environment.
-}
reader : value -> Reader env value
reader x =
    Reader <| always x


{-| Fetch the value of the environment.
-}
ask : Reader env env
ask =
    Reader <| identity


{-| Embed a function in a Reader
-}
asks : (env -> value) -> Reader env value
asks f =
    Reader f


{-| Locally modify the environment for the next Reader action.
-}
local : (env -> env) -> Reader env a -> Reader env a
local f (Reader action) =
    Reader (action << f)


{-| Apply a function to the resulting value of the Reader.
-}
map : (a -> b) -> Reader env a -> Reader env b
map f (Reader g) =
    Reader (f << g)


{-| Apply a function wrapped in a Reader to a value wrapped in a Reader.
This is particularly useful for transforming a succession of resulting values
with a single function.

    map (\x y z -> ( x, y, z )) (reader 'x')
        |> andMap (reader 42)
        |> andMap (reader "fourty two")

        == reader ('x', 42, "fourty two")

-}
andMap : Reader env a -> Reader env (a -> b) -> Reader env b
andMap =
    flip apply


{-| Chain a Reader with a computation producing a Reader
-}
andThen : (a -> Reader env b) -> Reader env a -> Reader env b
andThen =
    flip bind


{-| Discard one level of Reader
-}
join : Reader env (Reader env a) -> Reader env a
join x =
    x
        |> andThen identity



-- HELPERS


apply : Reader env (a -> b) -> Reader env a -> Reader env b
apply (Reader f) (Reader g) =
    Reader <| \env -> (f env (g env))


bind : Reader env a -> (a -> Reader env b) -> Reader env b
bind x f =
    Reader <|
        (\r ->
            run x r
                |> f
                |> (flip run) r
        )


flip : (a -> b -> c) -> b -> a -> c
flip f x y =
    f y x
