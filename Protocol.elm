module Protocol where

type alias Time = Int
type alias Duration = Int

time : Int -> Int -> Time
time h m = h

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
        incFood d f = { d | food <- d.food + f }
        incInsulin d u = { d | insulin <- d.insulin + u }
        inc event d =
            case event of
                Food time carbs ->
                    carbs * (response config.foodResponse (between time start, between time end))
                    |> incFood d
                Bolus time u ->
                    u * (response config.insulinResponse (between time start, between time end))
                    |> incInsulin d
    in
        List.foldr inc { food = 0, insulin = 0 } data


type Event
    = Food Time Float
    | Bolus Time Float
