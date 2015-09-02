module Main where

import Protocol exposing (..)
import Table
import Number.Format
import Html

config =
    { foodResponse = linear 2
    , insulinResponse = linear 5
    }

data =
    [ Food (time 9 12) 50
    , Bolus (time 9 12) 5.00
    , Food (time 10 58) 15
    , Bolus (time 10 58) 1.62
    , Food (time 12 29) 50
    , Bolus (time 12 29) 5.54
    , Bolus (time 14 40) 4.00
    , Food (time 17 20) 25
    , Bolus (time 17 20) 2.13
    , Bolus (time 20 18) 1.55
    , Food (time 21 15) 55
    , Bolus (time 21 15) 4.58
    , Food (time 21 37) 20
    , Bolus (time 21 37) 1.67
    , Food (time 21 52) 20
    , Bolus (time 21 52) 1.67
    ]

hourlyView =
    let
        p i = process config data (time i 0) (time (i+1) 0)
    in
        [0..24]
        |> Table.table
            [ ("Time", toString)
            , ("Food", p >> .food >> Number.Format.pretty 1 ',')
            , ("Insulin", p >> .insulin >> Number.Format.pretty 3 ',')
            ]

checks =
    [ (time 8 32, 142)
    , (time 14 46, 265)
    , (time 20 25, 179)
    ]

mapPairs : (a -> a -> b) -> List a -> List b
mapPairs fn input =
    let
        step next (last,acc) =
            case last of
                Nothing -> (Just next,acc)
                Just last' -> (Just next,(fn next last')::acc)
    in
        List.foldr step (Nothing,[]) input
        |> snd

checksView =
    checks
    |> mapPairs (\(ta,ga) (tb,gb) -> ((ga,gb),process config data ta tb))
    |> Table.table
        [ ("start BG", fst >> fst >> toString)
        , ("end BG", fst >> snd >> toString)
        , ("Food", snd >> .food >> Number.Format.pretty 1 ',')
        , ("Insulin", snd >> .insulin >> Number.Format.pretty 3 ',')
        ]

main =
    Html.div []
        [ checksView
        , hourlyView
        ]