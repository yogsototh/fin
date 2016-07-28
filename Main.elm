import Html exposing (Html, button, div, text, node)
import Html.Attributes exposing (style)
import Html.App as App
import Time exposing (Time,millisecond)
import Html.Events exposing (onClick)

main : Program Never
main = App.program { init = init
                   , view = view
                   , update = update
                   , subscriptions = subscriptions
                   }

-- MODEL

-- type alias NatResource = { name : String
--                          , growthBySec : Float
--                          , minBound : Float
--                          , maxBound : Float
--                          , value : Float
--                          }
-- 
-- humanity : NatResource
-- humanity = { name = "sane people"
--            , minBound = 0
--            , maxBound = 20000000000
--            , value = 7000000000
--            }
-- flu = { resourceName = "sane people"
--       , growthBySec = -0.00001
--       }
-- 
type alias Model = { money : Float
                   , mbs : Float
                   , t : Time
                   , timeSpeed : Int
                   , relTime : Time
                   }

init : (Model,Cmd Msg)
init = ({ money = 0
        , mbs = 1
        , relTime = 0
        , t = 0
        , timeSpeed = 1
        }, Cmd.none)

-- UPDATE

type Msg = Gain Int | Tick Time | Reset | ChangeTimeSpeed Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let newmodel = case msg of
                       Gain i -> { model | mbs = max 0 (model.mbs + (toFloat i)) }
                       ChangeTimeSpeed i -> { model | timeSpeed = max 0 i }
                       Tick newTime ->
                           { model | money = model.money +
                                             (if model.t > 0
                                              then model.mbs * (toFloat model.timeSpeed) * (newTime - model.t) / 1000
                                              else 0)
                           , t = newTime
                           , relTime = (if model.t > 0
                                        then model.relTime + (toFloat model.timeSpeed) * (newTime - model.t)
                                        else 0)}
                       Reset -> { model | money = 0 }
    in (newmodel,Cmd.none)

-- SUBSCRIPTION

subscriptions : Model -> Sub Msg
subscriptions model = Time.every millisecond Tick

wdgStyle : Html.Attribute msg
wdgStyle = style [ ("margin","1em auto")
                 , ("font-family","Helvetica")
                 , ("display","block")
                 , ("padding", "1em")
                 , ("font-size","16px")
                 , ("color", "#555")
                 , ("border","solid")
                 , ("width", "20em")
                 , ("text-align", "center")]

oneDay : Int
oneDay = 24 * oneHour
oneHour : Int
oneHour = 60 * oneMinute
oneMinute : Int
oneMinute = 60 * oneSecond
oneSecond : Int
oneSecond = 1000

addZero : Int -> String
addZero i = if i < 10 then "0" else ""

displayTime : Int -> String
displayTime t =
    let days = t // oneDay
        hours = (t % oneDay) // oneHour
        minutes = (t % oneHour) // oneMinute
        seconds = (t % oneMinute) // oneSecond
    in toString days ++ " "
        ++ (addZero hours) ++ toString hours ++ ":"
        ++ (addZero minutes) ++ toString minutes ++ ":"
        ++ (addZero seconds) ++ toString seconds


view : Model -> Html Msg
view model = div [ wdgStyle ] 
             [ div [ style [("color","#888")
                           , ("font-size","12px")]]
                   [text ("dbs: " ++ (toString model.mbs))]
             , div [ style [("color","#888")
                           , ("font-size","12px")]]
                   [text ("time: " ++ (displayTime <| truncate <| model.relTime))]

             , div [ style [("color","#888")
                           , ("font-size","12px")
                           , ("display","inline")]]
                 [text "time speed: "]
             , button [ onClick (ChangeTimeSpeed 1) ] [text "x1"]
             , button [ onClick (ChangeTimeSpeed 60) ] [text "1min/s"]
             , button [ onClick (ChangeTimeSpeed 3600) ] [text "1h/s"]
             , button [ onClick (ChangeTimeSpeed (24*3600)) ] [text "1d/s"]
             , node "hr" [] []
             , button [ onClick (Gain -1) ] [text "-1/s"]
             , button [ onClick (Gain 1) ] [text "+1/s"]
             , button [ onClick Reset ] [text "reset"]
             , div [ style [ ("font-size","21px")
                           , ("font-family","Menlo, monaco, monospace")
                           , ("background","#EEE")
                           , ("padding","1em")
                           , ("margin","1em")
                           , ("border","solid 1px #CCC")
                           ]]
                 [text (toString (truncate model.money))]
             ]
