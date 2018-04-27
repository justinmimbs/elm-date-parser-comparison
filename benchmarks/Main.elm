module Main exposing (main)

import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram)
import DateParsers


main : BenchmarkProgram
main =
    Benchmark.Runner.program suite


suite : Benchmark
suite =
    let
        input =
            [ "2008"
            , "200801"
            , "200812"
            , "20080101"
            , "20081231"
            , "2009W01"
            , "2009W014"
            , "2008001"
            , "2008061"
            , "2008366"
            , "2008-01"
            , "2008-12"
            , "2008-01-01"
            , "2008-12-31"
            , "2009-W01"
            , "2009-W01-4"
            , "2008-001"
            , "2008-061"
            , "2008-366"

            -- bad
            , "200812-31"
            , "2008-1231"
            , "2009W01-4"
            , "2009-W014"
            , "2008-012-31"
            , "2008-12-031"
            , "2008-0061"
            ]
    in
    Benchmark.compare "fromIsoString"
        "Parser"
        (\() -> input |> List.map DateParsers.fromIsoString_Parser)
        "Regex"
        (\() -> input |> List.map DateParsers.fromIsoString_Regex)
