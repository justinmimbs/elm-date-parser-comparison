module DateParsers
    exposing
        ( Date(..)
        , YearDate(..)
        , fromIsoString_Parser
        , fromIsoString_Regex
        )

import Char
import Parser exposing ((|.), (|=), Count(Exactly), Parser, delayedCommitMap, end, ignore, keyword, map, oneOf, source, succeed, symbol)
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
    Regex.find (Regex.AtMost 1) isoDateRegex
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
    Regex.regex <| "^" ++ year ++ "(?:" ++ cal ++ "|" ++ week ++ "|" ++ ord ++ ")?$"


fromIsoStringMatches : List (Maybe String) -> Result String Date
fromIsoStringMatches =
    let
        toInt : Maybe String -> Int
        toInt =
            Maybe.andThen (String.toInt >> Result.toMaybe) >> Maybe.withDefault 1
    in
    \matches ->
        case matches of
            [ Just yyyy, _, mn, d, _, wn, wdn, od ] ->
                Ok <|
                    Year
                        (yyyy |> String.toInt |> Result.withDefault 1)
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
fromIsoString_Parser =
    Parser.run (parseDate |. end) >> Result.mapError (always "String is not in IS0 8601 date format")


parseDate : Parser Date
parseDate =
    succeed Year
        |= intFixed 4
        |= oneOf
            [ succeed identity
                |. symbol "-"
                |= oneOf
                    [ delayedCommitMap MonthDate
                        (intFixed 2)
                        (succeed identity
                            |. symbol "-"
                            |= intFixed 2
                        )
                    , try <|
                        map OrdinalDate
                            (intFixed 3)
                    , succeed MonthDate
                        |= intFixed 2
                        |= succeed 1
                    , succeed WeekDate
                        |. keyword "W"
                        |= intFixed 2
                        |= oneOf
                            [ succeed identity
                                |. symbol "-"
                                |= intFixed 1
                            , succeed 1
                            ]
                    ]
            , try <|
                succeed MonthDate
                    |= intFixed 2
                    |= intFixed 2
            , try <|
                map OrdinalDate
                    (intFixed 3)
            , succeed MonthDate
                |= intFixed 2
                |= succeed 1
            , succeed WeekDate
                |. keyword "W"
                |= intFixed 2
                |= oneOf
                    [ intFixed 1
                    , succeed 1
                    ]
            , succeed (OrdinalDate 1)
            ]


try : Parser a -> Parser a
try p =
    delayedCommitMap always p (succeed ())


intFixed : Int -> Parser Int
intFixed width =
    map
        (String.toInt >> Result.withDefault 0)
        (ignore (Exactly width) Char.isDigit
            |> source
        )
