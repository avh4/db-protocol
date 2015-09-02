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
        process' = process { foodResponse = linear 2 }
    in suite "calculating food"
        [ test "no food" <|
            assertEqual (process' [] (time 9) (time 10) |> .food) 0
        , test "food fully released in the range" <|
            assertEqual (process' [ Food (time 9) 50 ] (time 0) (time 24) |> .food) 50
        , test "multiple foods combine" <|
            assertEqual (process' [ Food (time 9) 13, Food (time 10) 8] (time 0) (time 24) |> .food) 21
        , test "food absorption" <|
            assertEqual (process' [ Food (time 9) 100 ] (time 9) (time 10) |> .food) 50
        ]

all : Test
all =
    suite "all"
        [ linearTest
        , processTest
        ]
