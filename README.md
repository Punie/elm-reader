Reader
======

This library provide useful functions for working with the Reader type.
It is intended as a way to propagate an implicit environment parameter through
various computations, only providing said parameter when the final result is needed.

Consider the following example:

```elm
type Country = France | Spain | USA


language : Country -> String
language country =
    case country of
        France -> "Bonjour"
        Spain  -> "Buenos dìas"
        USA    -> "Hello"


greet : String -> Reader Country String
greet name =
    let
        doGreet greeting =
            reader <| greeting ++ ", " ++ name
    in
        asks language
            |> andThen doGreet


exclamation : String -> Reader Country String
exclamation input =
    let
        localExclamation country =
            case country of
                France ->
                    input ++ " !"
                Spain ->
                    "¡" ++ input ++ "!"
                USA ->
                    input ++ "!"
    in
        ask
            |> andThen localExclamation


greeting : String -> Reader Country String
greeting name =
    greet name |> andThen exclamation


run (greeting "Elm") France == "Bonjour, Elm !"
run (greeting "Elm") Spain  == "¡Buenos Dìas, Elm!"
run (greeting "Elm") USA    == "Hello, Elm!"
```

The `greet` function asks for the environment variable of type `Country` and transforms it to a `String`
via the `language` function. Through `andThen` this result is used to build the greeting sentence.

The `exclamation` function takes a `String` and asks for the environment of type `Country` and uses
that environment to decide how to write the exclamation mark(s).

Finally we run the same pre-built computation (`greet "Elm" |> andThen exclamation`) finally giving it once
and for all the parameter it expects.
