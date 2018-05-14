module Tests exposing (..)

--import Expect exposing (Expectation)
--import Test exposing (Test, describe, test)

import DateParsers exposing (Date(..), YearDate(..))
import Shim exposing (Expectation, Test, describe, equal, test)


test_fromIsoString : Test
test_fromIsoString =
    let
        toSuite : (String -> Result String Date) -> List Test
        toSuite fromIsoString =
            let
                toTest : ( String, Date ) -> Test
                toTest ( string, expected ) =
                    test (string ++ " => " ++ Debug.toString expected) <|
                        \() -> fromIsoString string |> equal (Ok expected)
            in
            [ describe "converts ISO 8601 date strings in basic format" <|
                List.map toTest
                    [ ( "2008", Year 2008 <| OrdinalDate 1 )
                    , ( "200801", Year 2008 <| MonthDate 1 1 )
                    , ( "200812", Year 2008 <| MonthDate 12 1 )
                    , ( "20080101", Year 2008 <| MonthDate 1 1 )
                    , ( "20081231", Year 2008 <| MonthDate 12 31 )
                    , ( "2009W01", Year 2009 <| WeekDate 1 1 )
                    , ( "2009W014", Year 2009 <| WeekDate 1 4 )
                    , ( "2008001", Year 2008 <| OrdinalDate 1 )
                    , ( "2008061", Year 2008 <| OrdinalDate 61 )
                    , ( "2008366", Year 2008 <| OrdinalDate 366 )
                    ]
            , describe "converts ISO 8601 date strings in extended format" <|
                List.map toTest
                    [ ( "2008-01", Year 2008 <| MonthDate 1 1 )
                    , ( "2008-12", Year 2008 <| MonthDate 12 1 )
                    , ( "2008-01-01", Year 2008 <| MonthDate 1 1 )
                    , ( "2008-12-31", Year 2008 <| MonthDate 12 31 )
                    , ( "2009-W01", Year 2009 <| WeekDate 1 1 )
                    , ( "2009-W01-4", Year 2009 <| WeekDate 1 4 )
                    , ( "2008-001", Year 2008 <| OrdinalDate 1 )
                    , ( "2008-061", Year 2008 <| OrdinalDate 61 )
                    , ( "2008-366", Year 2008 <| OrdinalDate 366 )
                    ]
            , describe "returns Err for malformed date strings" <|
                List.map
                    (\s -> test s <| \() -> fromIsoString s |> equal (Err "String is not in IS0 8601 date format"))
                    [ "200812-31"
                    , "2008-1231"
                    , "2009W01-4"
                    , "2009-W014"
                    , "2008-012-31"
                    , "2008-12-031"
                    , "2008-0061"
                    , "2018-05-1"
                    , "2018-5"
                    , "20180"
                    ]
            ]
    in
    describe "fromIsoString"
        [ describe "Parser" <| toSuite DateParsers.fromIsoString_Parser
        , describe "Regex" <| toSuite DateParsers.fromIsoString_Regex
        ]
