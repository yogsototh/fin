import Html exposing (Html, button, div, text)
import Html.App as App
import Time exposing (Time,second)
import Html.Events exposing (onClick)

main : Program Never
main = App.program { init = init
                   , view = view
                   , update = update
                   , subscriptions = subscriptions
                   }

-- MODEL

type alias Model = { money : Int
                   , t : Time
                   }

init : (Model,Cmd Msg)
init = ({ money = 0
        , t = 0
        }, Cmd.none)

-- UPDATE

type Msg = Gain Int | Tick Time | Reset

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let newmodel = case msg of
                       Gain i -> { model | money = (model.money + i) }
                       Tick newTime -> { model | money = model.money + (if model.t > 0
                                                                        then truncate ((newTime - model.t) / 1000)
                                                                        else 0)
                                       , t = newTime }
                       Reset -> { model | money = 0 }
    in (newmodel,Cmd.none)

-- SUBSCRIPTION

subscriptions : Model -> Sub Msg
subscriptions model = Time.every second Tick

view : Model -> Html Msg
view model = div []
             [ button [ onClick (Gain 1) ] [text "+1"]
             , button [ onClick (Gain 2) ] [text "+2"]
             , div [] [text (toString (.money model))]
             , button [ onClick Reset ] [text "0"]
             ]
