module Tests where

import ElmTest.Assertion exposing (..)
import ElmTest.Test exposing (..)

import Protocol exposing (..)

linearTest =
    suite "linear"
        [ test "starts at 0.0" <| assertEqual (linear 10 0) 0.0
        , test "ends at 1.0" <| assertEqual (linear 10 10) 1.0
        , test "clamps at 1.0" <| assertEqual (linear 10 11) 1.0
        , test "clamps at 0.0" <| assertEqual (linear 10 -1) 0.0
        , test "calculates the midpoint" <| assertEqual (linear 10 5) 0.5
        ]

processTest =
    let
        process' = process { foodResponse = linear 2, insulinResponse = linear 5 }
        check fn (h0,h1) data e = assertEqual (process' data (time h0 0) (time h1 0) |> fn) e
    in suite "calculating food"
        [ test "no food" <|
            check .food (9,10) [] 0
        , test "food fully released in the range" <|
            check .food (0,24) [ Food (time 9 0) 50 ] 50
        , test "multiple foods combine" <|
            check .food (0,24) [ Food (time 9 0) 13, Food (time 10 0) 8 ] 21
        , test "recent food absorption" <|
            check .food (9,10) [ Food (time 9 0) 100 ] 50
        , test "old food absorption" <|
            check .food (10,11) [ Food (time 9 0) 100 ] 50
        , test "when food is later" <|
            check .food (8,9) [ Food (time 9 0) 100 ] 0

        , test "no bolus" <|
            check .insulin (9,10) [] 0
        , test "bolus fully released in the range" <|
            check .insulin (0,24) [ Bolus (time 9 0) 1.0 ] 1.0
        ]

all : Test
all =
    suite "all"
        [ linearTest
        , processTest
        ]
