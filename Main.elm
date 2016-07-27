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

type alias Model = { money : Float
                   , mbs : Float
                   , t : Time
                   }

init : (Model,Cmd Msg)
init = ({ money = 0
        , mbs = 1
        , t = 0
        }, Cmd.none)

-- UPDATE

type Msg = Gain Int | Tick Time | Reset

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let newmodel = case msg of
                       Gain i -> { model | mbs = max 0 (model.mbs + (toFloat i)) }
                       Tick newTime -> { model |
                            money = model.money +
                                    (if model.t > 0
                                     then model.mbs * (newTime - model.t) / 1000
                                     else 0)
                                       , t = newTime }
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

view : Model -> Html Msg
view model = div [ wdgStyle ] 
             [ div [ style [("color","#888")
                           , ("font-size","12px")]] [text ("dbs: " ++ (toString model.mbs))]
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
