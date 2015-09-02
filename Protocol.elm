module Protocol where

type alias Time = Int
type alias Duration = Int

time : Int -> Time
time h = h

between : Time -> Time -> Duration
between a b = (b - a)

type alias Impulse = Duration -> Float

response : Impulse -> (Duration, Duration) -> Float
response impulse (start, end) =
    (impulse end) - (impulse start)

linear : Duration -> Impulse
linear length t = (toFloat t) / (toFloat length) |> max 0 |> min 1

process config data start end =
    let
        inc event d =
            case event of
                Food time carbs ->
                    { d
                    | food <- d.food + carbs * (response config.foodResponse (between time start, between time end))
                    }
    in
        List.foldr inc { food = 0 } data


type Event
    = Food Time Float
