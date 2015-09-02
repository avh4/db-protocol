module Main where

import Protocol exposing (..)
import Table

config =
    { foodResponse = linear 2 }

data =
    [ Food (time  9) 50
    , Food (time 11) 15
    , Food (time 12) 50
    , Food (time 17) 25
    ]

main =
    let
        p i = process config data (time i) (time <| i+1)
    in
        [0..24]
        |> Table.table
            [ ("Time", toString)
            , ("Food", p >> .food >> toString)
            ]
