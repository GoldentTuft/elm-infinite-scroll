module InfScroll exposing
    ( InfScroll
    , Status(..)
    , Dir(..)
    , Msg
    , DomId
    , init
    , setDir
    , setOffset
    , setInterval
    , subscriptions
    , update
    , whenLoadMore
    , toLoaded
    , toStop
    , view
    , defaultContainerAttrs
    , defaultItemsAttrs
    , getDir
    , getStatus
    , getViewport
    )

{-|


# Type

@docs InfScroll
@docs Status
@docs Dir
@docs Msg
@docs DomId


# Init

@docs init
@docs setDir
@docs setOffset
@docs setInterval


# Update

@docs subscriptions
@docs update
@docs whenLoadMore
@docs toLoaded
@docs toStop


# View

@docs view
@docs defaultContainerAttrs
@docs defaultItemsAttrs


# Get

@docs getDir
@docs getStatus
@docs getViewport

-}

import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (id, style)
import Task
import Time


{-| -}
type InfScroll
    = InfScroll InfScrollInternal


type alias InfScrollInternal =
    { domId : DomId
    , status : Status
    , dir : Dir
    , offset : Int
    , interval : Float
    , prevViewport : Maybe Dom.Viewport
    , viewport : Maybe Dom.Viewport
    }


{-| -}
type Dir
    = Top
    | Bottom


{-| -}
type Status
    = Loading
    | Loaded
    | Loadable
    | Stop


{-| -}
type alias DomId =
    String


{-|

    InfScroll.init "item-list" InfScroll.Bottom
        |> setOffset 100

-}
init : DomId -> Dir -> InfScroll
init domId dir =
    InfScroll
        { domId = domId
        , status = Loading
        , dir = dir
        , offset = 50
        , interval = 100
        , prevViewport = Nothing
        , viewport = Nothing
        }


{-| -}
type Msg
    = TimeEvery Time.Posix
    | GotViewport (Result Dom.Error Dom.Viewport)


{-| -}
subscriptions : InfScroll -> Sub Msg
subscriptions (InfScroll infScroll) =
    Time.every infScroll.interval TimeEvery


{-| -}
update : Msg -> InfScroll -> ( InfScroll, Cmd Msg )
update msg (InfScroll infScroll) =
    case msg of
        TimeEvery _ ->
            ( InfScroll infScroll
            , Dom.getViewportOf infScroll.domId
                |> Task.attempt GotViewport
            )

        GotViewport res ->
            case res of
                Err _ ->
                    ( InfScroll infScroll, Cmd.none )

                Ok info ->
                    let
                        newInfScroll =
                            { infScroll
                                | prevViewport = infScroll.viewport
                                , viewport = Just info
                            }
                    in
                    ( InfScroll
                        { newInfScroll
                            | status =
                                if not <| isInOffset newInfScroll then
                                    case newInfScroll.status of
                                        Loaded ->
                                            Loadable

                                        _ ->
                                            newInfScroll.status

                                else
                                    newInfScroll.status
                        }
                    , Cmd.none
                    )


offset : InfScrollInternal -> Maybe Int
offset infScroll =
    Maybe.map
        (\info ->
            round <| info.scene.height - (abs info.viewport.y + info.viewport.height)
        )
        infScroll.viewport


isInOffset : InfScrollInternal -> Bool
isInOffset infScroll =
    case offset infScroll of
        Nothing ->
            False

        Just offset_ ->
            offset_ <= infScroll.offset


isScrollStop : InfScrollInternal -> Bool
isScrollStop infScroll =
    case ( infScroll.prevViewport, infScroll.viewport ) of
        ( Just prevViewport, Just viewport ) ->
            prevViewport.viewport == viewport.viewport

        _ ->
            False


{-|

    MapInfScroll subMsg ->
        let
            (newInfScroll, cmd) =
                InfScroll.update subMsg model.infScroll
                    |> InfScroll.whenLoadMore MapInfScroll getItems
        in
        ( { model | infScroll = newInfScroll }
        , cmd
        )

-}
whenLoadMore : (Msg -> msg) -> Cmd msg -> ( InfScroll, Cmd Msg ) -> ( InfScroll, Cmd msg )
whenLoadMore mapper loadMore ( InfScroll infScroll, cmd ) =
    let
        otherwise =
            ( InfScroll infScroll, Cmd.map mapper cmd )
    in
    if isInOffset infScroll && isScrollStop infScroll then
        case infScroll.status of
            Loadable ->
                ( InfScroll
                    { infScroll
                        | status = Loading
                    }
                , Cmd.batch
                    [ Cmd.map mapper cmd
                    , loadMore
                    ]
                )

            _ ->
                otherwise

    else
        case infScroll.status of
            Loaded ->
                ( InfScroll
                    { infScroll
                        | status = Loadable
                    }
                , Cmd.map mapper cmd
                )

            _ ->
                otherwise


{-|

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

-}
toLoaded : InfScroll -> InfScroll
toLoaded (InfScroll infScroll) =
    InfScroll { infScroll | status = Loaded }


{-| -}
toStop : InfScroll -> InfScroll
toStop (InfScroll infScroll) =
    InfScroll { infScroll | status = Stop }


{-| -}
setDir : Dir -> InfScroll -> InfScroll
setDir newDir (InfScroll infScroll) =
    InfScroll { infScroll | dir = newDir }


{-| -}
setOffset : Int -> InfScroll -> InfScroll
setOffset newOffset (InfScroll infScroll) =
    InfScroll { infScroll | offset = newOffset }


{-| -}
setInterval : Float -> InfScroll -> InfScroll
setInterval newInterval (InfScroll infScroll) =
    InfScroll { infScroll | interval = newInterval }


{-| -}
getStatus : InfScroll -> Status
getStatus (InfScroll infScroll) =
    infScroll.status


{-| -}
getDir : InfScroll -> Dir
getDir (InfScroll infScroll) =
    infScroll.dir


{-| -}
getViewport : InfScroll -> Maybe Dom.Viewport
getViewport (InfScroll infScroll) =
    infScroll.viewport


{-| -}
defaultContainerAttrs : InfScroll -> List (Attribute msg)
defaultContainerAttrs (InfScroll infScroll) =
    [ style "display" "flex"
    , case infScroll.dir of
        Top ->
            style "flex-direction" "column-reverse"

        Bottom ->
            style "flex-direction" "column"
    , style "overflow" "auto"
    ]


{-| -}
defaultItemsAttrs : InfScroll -> List (Attribute msg)
defaultItemsAttrs (InfScroll infScroll) =
    [ style "display" "flex"
    , case infScroll.dir of
        Top ->
            style "flex-direction" "column-reverse"

        Bottom ->
            style "flex-direction" "column"
    , style "overflow" "initial"
    ]


{-|

    InfScroll.view model.infScroll
        (InfScroll.defaultContainerAttrs infScroll)
        (InfScroll.defaultItemsAttrs infScroll)
        (List.map viewItem model.items)

-}
view : InfScroll -> List (Attribute msg) -> List (Attribute msg) -> List (Html msg) -> Html msg
view (InfScroll infScroll) attrs1 attrs2 children =
    div
        (id infScroll.domId :: attrs1)
        [ div
            attrs2
            children
        ]
