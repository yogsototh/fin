import Html exposing (Html, button, div, text, node)
import Html.Attributes exposing (style)
import Html.App as App
import Time exposing (Time,millisecond)
import Html.Events exposing (onClick)
-- import List exposing (map, foldl, head, tail)
import Set as Set
import Dict as Dict
-- import Maybe exposing (withDefault)

main : Program Never
main = App.program { init = init
                   , view = view
                   , update = update
                   , subscriptions = subscriptions
                   }

-- MODEL

type alias NeededResource =
    { resourceName : String
    , neededQuantity : Float
    , period : Time
    -- variance of resource access by in need resource unity
    , variance : Float
    -- death probability for each needed resource
    -- that don't reach its consumption goal
    , deathProbability : Float
    }
    
type alias NatResource =
    { name : String
    , quantity: Float
    , stdGrowBySecByUnit : (Float,Float)
    , neededResources : Set.Set NeededResource
    }
        
year : Float
year = 1000 * 60 * 60 * 24 * 365

humanity : NatResource
humanity = { name = "humanity"
           , quantity = 7000000000
           , stdGrowBySecByUnit = (3,30 * year)
           , neededResources = Set.empty
           }

type alias Model = { resources : Dict.Dict String NatResource
                   , t : Time
                   , timeSpeed : Int
                   , relTime : Time
                   }

init : (Model,Cmd Msg)
init = (initModel, Cmd.none)

initModel : Model
initModel =
    { resources = initResources
    , relTime = 0
    , t = 0
    , timeSpeed = 1
    }
    
initResources : Dict.Dict String NatResource
initResources = Dict.fromList (List.map (\h -> (h.name,h)) [humanity])

-- UPDATE

type Msg = Tick Time | Reset | ChangeTimeSpeed Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let newmodel = case msg of
                       ChangeTimeSpeed i -> { model | timeSpeed = max 0 i }
                       Tick newTime ->
                           let elapsedTimeInMs = (if model.t > 0
                                                  then (toFloat model.timeSpeed) * (newTime - model.t)
                                                  else 0)
                           in { model
                                  | resources = Dict.map (updateResource model.resources elapsedTimeInMs) model.resources
                                  , t = newTime
                                  , relTime = (if model.t > 0
                                               then model.relTime + (toFloat model.timeSpeed) * (newTime - model.t)
                                               else 0)}
                       Reset -> fst init
    in (newmodel,Cmd.none)

updateResource : Dict.Dict String NatResource -> Float -> String -> NatResource -> NatResource
updateResource allResources elapsedTimeInMs resourceName natResource =
    let growRatio = (fst natResource.stdGrowBySecByUnit) / (snd natResource.stdGrowBySecByUnit)
    in { natResource | quantity = natResource.quantity + natResource.quantity * growRatio
       }

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
             ([ div [ style [("color","#888")
                            , ("font-size","12px")]]
                    [text ("dbs: " ++ (toString model.resources))]
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
              , button [ onClick Reset ] [text "reset"]]
                  ++
                  (Dict.toList model.resources |>
                       List.map snd |>
                       List.map viewResource))
                 
viewResource : NatResource -> Html Msg
viewResource resource = 
    div [ style [ ("font-size","21px")
                , ("font-family","Menlo, monaco, monospace")
                , ("background","#EEE")
                , ("padding","1em")
                , ("margin","1em")
                , ("border","solid 1px #CCC")
                ]]
        [ Html.span [style [ ("font-weight","bold")
                           , ("font-size",".8em")
                           , ("margin-bottom","2ex")]]
              [text <| (resource.name) ++ ": "]
        , resource.quantity |> round |> toString |> text ]
