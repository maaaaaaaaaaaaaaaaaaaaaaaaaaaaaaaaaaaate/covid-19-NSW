module Main exposing (main)

import Browser
import Html exposing (Html, text, div, input, label, button, option, select, p, strong)
import Html.Attributes exposing (class, type_, placeholder, value, checked, id, attribute)
import Html.Events exposing (onClick, onInput, onCheck)
import Time exposing (Posix, posixToMillis, millisToPosix)
import Json.Decode exposing (Decoder, field, string, list, at, int, dict, maybe, map)
import Json.Encode exposing (encode, object)
import Http exposing (Error)
import Dict exposing (Dict)
import Iso8601 exposing (toTime, fromTime)

main = Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type Msg = IncDate Int
         | PlayTimeline
         | JsonResponse (Result Http.Error (List Properties))
         | CheckDate String
         | CheckPostcode String
         | CheckSources String Bool
         | UpdateDate String

type alias Model =
    { dates:
          { date : Posix
          , minDate : Posix
          , maxDate : Posix
          }
    , numDays : Maybe Int
    , sources : Dict String Bool
    , filtered : Dict String Int
    , unfiltered : List Properties
    , postcode : String
    , dateVal : String
    , isPlaying : Bool
    }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model { date = Time.millisToPosix 0
          , minDate = Time.millisToPosix 0
          , maxDate = Time.millisToPosix 0}
        Nothing Dict.empty Dict.empty [] "" "" False
  , Cmd.batch [ Http.get
                   { url = "/geo.json"
                   , expect = Http.expectJson JsonResponse geoDecoder}
              ]
  )

type alias Properties =
    { postcode : String
    , infections : Dict String (Dict String Int)
    }



geoDecoder = field "features"
             <| list (Json.Decode.map2 Properties
                          (field "properties"
                               <| field "postcode" string)
                          (field "properties"
                               <| field "infections" <| dict <| dict <| int))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let dates = model.dates in
    case msg of
        IncDate sign -> let newDate = incDay dates sign in
                        ({ model
                             | dates = { dates | date = newDate }
                             , dateVal = String.left 10 (fromTime newDate)
                             , isPlaying = if model.isPlaying && newDate == dates.maxDate
                                           then False
                                           else model.isPlaying }
                             |> refilter
                        , Cmd.none)
        PlayTimeline -> (if not model.isPlaying && model.dates.date == model.dates.maxDate
                         then { model
                                  | isPlaying = not model.isPlaying,
                                    dates = { dates | date = model.dates.minDate} }
                             |> refilter
                         else { model | isPlaying = not model.isPlaying }
                        , Cmd.none)
        JsonResponse result ->
            case Result.toMaybe result of
                Nothing -> (model, Cmd.none)
                Just geoJson ->
                    (let newMax = maxDateFold geoJson
                     in {model
                            | unfiltered = geoJson
                            , sources = infectionTypes geoJson
                            , dates = { dates
                                          | minDate = minDateFold geoJson
                                          , maxDate = newMax
                                          , date = newMax }
                            , dateVal = String.left 10 <| fromTime newMax }
                         |> refilter
                    , Cmd.none)
        CheckDate string -> case Result.toMaybe <| toTime string of
                                Nothing -> ({model | dateVal = string}
                                           , Cmd.none)
                                Just posix -> ({model
                                                   | dates = { dates | date = posix}
                                                   , dateVal = string }
                                                   |> refilter
                                              , Cmd.none)
        CheckPostcode postcode -> ({model | postcode = postcode }, Cmd.none)
        CheckSources name bool -> ({model | sources = Dict.insert name bool model.sources} |> refilter, Cmd.none)
        UpdateDate val -> ( { model | numDays = String.toInt val} |> refilter
                          , Cmd.none)
refilter : Model -> Model
refilter model =
    let newFilter = List.foldl checkProps Dict.empty model.unfiltered
        checkProps prop result1 = Dict.insert prop.postcode (Dict.foldl checkDates 0 prop.infections) result1
        checkDates date infs result2 =
            case toTime date |> Result.toMaybe of
                Nothing -> result2
                Just posix -> if posixToMillis posix >= (case model.numDays of
                                                             Nothing -> posixToMillis model.dates.minDate
                                                             Just num -> posixToMillis model.dates.date  - num * 86400000)
                              && posixToMillis posix <= posixToMillis model.dates.date
                              then Dict.foldl checkLast result2 infs
                              else result2
        checkLast inf num result3 = if Dict.isEmpty model.sources
                                    || case Dict.get inf model.sources of
                                           Just v -> v
                                           Nothing -> False
                                    then num + result3
                                    else result3
    in { model
           | filtered = newFilter }

subscriptions : Model -> Sub Msg
subscriptions model = if model.isPlaying
                      then always (IncDate 1) |> Time.every 500
                      else Sub.none

jsonEncode val = encode 0 <| object <| Dict.toList <| Dict.map (\_ -> Json.Encode.int) val

view : Model -> Html Msg
view model =
    let postcodeCheck = List.any (\p -> model.postcode == p.postcode) model.unfiltered
    in div [class "box", id "filtered_json", attribute "data-json" (jsonEncode model.filtered)]
             [ div [class "field has-addons"]
                   [ div [class "control"] [button [class "button is-dark", onClick (IncDate -1)] [text "Prev"]]
                   , div [class "control"] [input [ case toTime model.dateVal |> Result.toMaybe of
                                                    Nothing -> class "input is-danger"
                                                    Just _ -> class "input"
                                                  , type_ "text"
                                                  , onInput CheckDate
                                                  , value model.dateVal] []]
                   , div [class "control"] [button [class "button is-dark", onClick (IncDate 1)] [text "Next"]]
                   ]
             , formField "Timeline" [if model.isPlaying
                                     then playButton "Pause" "button is-warning"
                                     else playButton "Play" "button is-success"]
             , formField "Postcode" [input [ if model.postcode == "" then class "input"
                                             else if postcodeCheck
                                                  then class "input is-success"
                                                  else class "input is-danger"
                                           , type_ "text"
                                           , onInput CheckPostcode
                                           , placeholder "Postcode"] []]
             , if postcodeCheck
               then postcodeDetails model.postcode model.unfiltered
               else div [] []
             , formField "Cases in last n days" [div [class "select"] [select [onInput UpdateDate] (manyOptions model.dates)]]
             , formField "Filter" (List.map formCheckbox <| Dict.toList model.sources)
             ]

showFiltered postcode num = p [] [postcode ++ ": " |> text, strong [] [String.fromInt num |> text]]

manyOptions dates =
    let numOpts = (posixToMillis dates.date - posixToMillis dates.minDate) // 86400000 + 1
        optionsTill num = if num > numOpts
                          then []
                          else option [] [num |> String.fromInt |> text] :: (optionsTill <| num + 1)
    in option [] [text "any"] :: (optionsTill 1)

infectionTypes data =
    let checkProps prop result1 = Dict.foldl checkDates result1 prop.infections
        checkDates date infs result2 = Dict.foldl checkLast result2 infs
        checkLast inf num result3 = if Dict.member inf result3
                                    then result3
                                    else Dict.insert inf True result3
    in List.foldl checkProps Dict.empty data

minDateFold data =
    List.foldl (\p -> \r ->
                    case p.infections |> Dict.keys |> List.head of
                        Just v -> if v < r then v else r
                        Nothing -> r)
        "9999-99-99" data
            |> \s -> case toTime s |> Result.toMaybe of
                         Just posix -> posix
                         Nothing -> millisToPosix 0

maxDateFold data =
    List.foldl (\p -> \r ->
                    case p.infections |> Dict.keys |> List.reverse |> List.head of
                        Just v -> if v > r then v else r
                        Nothing -> r)
        "0000-00-00" data
            |> \s -> case toTime s |> Result.toMaybe of
                         Just posix -> posix
                         Nothing -> millisToPosix 0

showProp property = p [] [text property.postcode]

postcodeDetails postcode properties =
    let datePrint date val = div [] <| strong [] [text date] :: (Dict.values <| Dict.map infPrint val)
        infPrint inf num = p [] [inf ++ ": " |> text, strong [] [String.fromInt num |> text]]
    in case List.head <| List.filter (\p -> postcode == p.postcode) properties of
        Nothing -> div [] []
        Just prop -> div [] (Dict.values <| Dict.map datePrint prop.infections)

playButton name classes = button [class classes, onClick PlayTimeline] [text name]

formField name htmls = div [class "field"]
                       [ label [class "label"] [text name]
                       , div [class "control"] htmls]

formCheckbox (description, checke) = div [class "field"]
                          [ div [class "control"]
                                [ label [class "checkbox"]
                                      [input [type_ "checkbox", checked checke, onCheck <| CheckSources description] []
                                      , text description]]]

-- Add or subtract one day from dates.date
incDay dates sign = let maxM = posixToMillis dates.maxDate
                        minM = posixToMillis dates.minDate
                        newDayM = posixToMillis dates.date + sign * 86400000
                    -- check bounds
                    in if newDayM > maxM then millisToPosix maxM
                       else if newDayM < minM then millisToPosix minM
                            else millisToPosix newDayM
