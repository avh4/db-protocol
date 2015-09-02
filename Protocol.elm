module Protocol where

type alias Time = Int
type alias Duration = Int

time : Int -> Time
time h = h

between : Time -> Time -> Maybe Duration
between a b =
    if b > a then Just (b - a) else Nothing

type alias Impulse = Duration -> Float

linear : Duration -> Impulse
linear length t = (toFloat t) / (toFloat length) |> max 0 |> min 1

process config data start end =
    let
        inc event d =
            case event of
                Food time carbs -> { d | food <- d.food + carbs * (between time end |> Maybe.map config.foodResponse |> Maybe.withDefault 0) }
    in
        List.foldr inc { food = 0 } data


type Event
    = Food Time Float
