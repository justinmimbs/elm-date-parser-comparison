module DateParsers
    exposing
        ( Date(..)
        , YearDate(..)
        , fromIsoString_Parser
        , fromIsoString_Regex
        )

import Char
import Parser exposing ((|.), (|=), Parser)
import Regex exposing (Regex)


type Date
    = Year Int YearDate


type YearDate
    = MonthDate Int Int
    | WeekDate Int Int
    | OrdinalDate Int



-- Regex


fromIsoString_Regex : String -> Result String Date
fromIsoString_Regex =
    Regex.find isoDateRegex
        >> List.head
        >> Result.fromMaybe "String is not in IS0 8601 date format"
        >> Result.andThen (.submatches >> fromIsoStringMatches)


isoDateRegex : Regex
isoDateRegex =
    let
        year =
            -- yyyy
            -- 1
            "(\\d{4})"

        cal =
            --       mm            dd
            -- 2     3             4
            "(\\-)?(\\d{2})(?:\\2(\\d{2}))?"

        week =
            --        ww            d
            -- 5      6             7
            "(\\-)?W(\\d{2})(?:\\5(\\d))?"

        ord =
            --     ddd
            --     8
            "\\-?(\\d{3})"
    in
    ("^" ++ year ++ "(?:" ++ cal ++ "|" ++ week ++ "|" ++ ord ++ ")?$")
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


fromIsoStringMatches : List (Maybe String) -> Result String Date
fromIsoStringMatches =
    let
        toInt : Maybe String -> Int
        toInt =
            Maybe.andThen String.toInt >> Maybe.withDefault 1
    in
    \matches ->
        case matches of
            [ Just yyyy, _, mn, d, _, wn, wdn, od ] ->
                Ok <|
                    Year
                        (yyyy |> String.toInt |> Maybe.withDefault 1)
                        (case ( mn, wn ) of
                            ( Just _, Nothing ) ->
                                MonthDate (mn |> toInt) (d |> toInt)

                            ( Nothing, Just _ ) ->
                                WeekDate (wn |> toInt) (wdn |> toInt)

                            _ ->
                                OrdinalDate (od |> toInt)
                        )

            _ ->
                Err "Unexpected results from isoDateRegex"



-- Parser


fromIsoString_Parser : String -> Result String Date
fromIsoString_Parser string =
    Parser.run date string |> Result.mapError (\_ -> "String is not in IS0 8601 date format")


date =
    Parser.succeed Year
        |= int4
        |= yearDate
        |. Parser.end


yearDate : Parser YearDate
yearDate =
    Parser.oneOf
        [ Parser.succeed identity
            -- extended format
            |. Parser.token "-"
            |= Parser.oneOf
                [ Parser.backtrackable
                    (Parser.map OrdinalDate
                        int3
                        |> Parser.andThen Parser.commit
                    )
                , Parser.succeed MonthDate
                    |= int2
                    |= Parser.oneOf
                        [ Parser.succeed identity
                            |. Parser.token "-"
                            |= int2
                        , Parser.succeed 1
                        ]
                , Parser.succeed WeekDate
                    |. Parser.token "W"
                    |= int2
                    |= Parser.oneOf
                        [ Parser.succeed identity
                            |. Parser.token "-"
                            |= int1
                        , Parser.succeed 1
                        ]
                ]

        -- basic format
        , Parser.backtrackable
            (Parser.succeed MonthDate
                |= int2
                |= Parser.oneOf
                    [ int2
                    , Parser.succeed 1
                    ]
                |> Parser.andThen Parser.commit
            )
        , Parser.map OrdinalDate
            int3
        , Parser.succeed WeekDate
            |. Parser.token "W"
            |= int2
            |= Parser.oneOf
                [ int1
                , Parser.succeed 1
                ]
        , Parser.succeed
            (OrdinalDate 1)
        ]


int4 : Parser Int
int4 =
    Parser.succeed ()
        |. Parser.chompIf Char.isDigit
        |. Parser.chompIf Char.isDigit
        |. Parser.chompIf Char.isDigit
        |. Parser.chompIf Char.isDigit
        |> Parser.mapChompedString
            (\str _ -> String.toInt str |> Maybe.withDefault 0)


int3 : Parser Int
int3 =
    Parser.succeed ()
        |. Parser.chompIf Char.isDigit
        |. Parser.chompIf Char.isDigit
        |. Parser.chompIf Char.isDigit
        |> Parser.mapChompedString
            (\str _ -> String.toInt str |> Maybe.withDefault 0)


int2 : Parser Int
int2 =
    Parser.succeed ()
        |. Parser.chompIf Char.isDigit
        |. Parser.chompIf Char.isDigit
        |> Parser.mapChompedString
            (\str _ -> String.toInt str |> Maybe.withDefault 0)


int1 : Parser Int
int1 =
    Parser.chompIf Char.isDigit
        |> Parser.mapChompedString
            (\str _ -> String.toInt str |> Maybe.withDefault 0)
