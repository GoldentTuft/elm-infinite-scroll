module Main exposing (Model, Msg, main)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck)
import InfScroll exposing (InfScroll)
import Process
import Task


type alias Model =
    { infScroll : InfScroll
    , items : List Item
    }


type alias Item =
    Int


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { infScroll =
            InfScroll.init itemsDomId InfScroll.Bottom
      , items = []
      }
    , Task.perform GotItems (Task.succeed <| List.range 0 20)
    )


type Msg
    = NoOp
    | MapInfScroll InfScroll.Msg
    | GotItems (List Item)
    | CheckReverse Bool


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map MapInfScroll <| InfScroll.subscriptions model.infScroll


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MapInfScroll subMsg ->
            let
                max =
                    List.maximum model.items
                        |> Maybe.withDefault 0

                fakeCmd =
                    Process.sleep 1000
                        |> Task.andThen
                            (\_ -> Task.succeed (List.range (max + 1) (max + 10)))
                        |> Task.perform GotItems

                ( newInfScroll, cmd ) =
                    InfScroll.update subMsg model.infScroll
                        |> InfScroll.whenLoadMore MapInfScroll fakeCmd
            in
            ( { model | infScroll = newInfScroll }
            , cmd
            )

        GotItems items ->
            ( { model
                | items =
                    List.sort (model.items ++ items)
                , infScroll =
                    if List.isEmpty items then
                        InfScroll.toStop model.infScroll

                    else
                        InfScroll.toLoaded model.infScroll
              }
            , Cmd.none
            )

        CheckReverse check ->
            let
                dir =
                    if check then
                        InfScroll.Top

                    else
                        InfScroll.Bottom
            in
            ( { model
                | infScroll =
                    InfScroll.setDir dir model.infScroll
              }
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    { title = "example"
    , body =
        [ label []
            [ input
                [ type_ "checkbox"
                , case InfScroll.getDir model.infScroll of
                    InfScroll.Top ->
                        checked True

                    InfScroll.Bottom ->
                        checked False
                , onCheck CheckReverse
                ]
                []
            , text "reverse"
            ]
        , viewItems model.infScroll model.items
        ]
    }


itemsDomId : String
itemsDomId =
    "items"


viewItems : InfScroll -> List Item -> Html Msg
viewItems infScroll items =
    let
        containerAttrs =
            [ style "border" "2px solid black"
            , style "max-height" "300px"
            ]
                ++ InfScroll.defaultContainerAttrs infScroll
    in
    div [ style "position" "relative" ]
        [ InfScroll.view infScroll
            containerAttrs
            (InfScroll.defaultItemsAttrs infScroll)
            (List.map viewItem items)
        , viewLoading infScroll
        ]


viewItem : Item -> Html Msg
viewItem item =
    div [] [ text <| String.fromInt item ]


viewLoading : InfScroll -> Html Msg
viewLoading infScroll =
    case InfScroll.getStatus infScroll of
        InfScroll.Loading ->
            div
                [ style "position" "absolute"
                , style "left" "50%"
                , case InfScroll.getDir infScroll of
                    InfScroll.Top ->
                        style "top" "0"

                    InfScroll.Bottom ->
                        style "bottom" "0"
                , style "transform" "translateX(-50%)"
                ]
                [ text "loading..." ]

        _ ->
            text ""
