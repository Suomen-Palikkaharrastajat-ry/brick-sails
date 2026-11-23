module Pages.Build exposing
    ( Model
    , Msg(..)
    , Outcome(..)
    , document
    , init
    , resize
    , setLanguage
    , subscriptions
    , update
    , view
    )

{-| The ship editor: a small Brick Play builder for one ship.

It wraps the shared 3D builder `Studio.Core` as Brick Play does, with a
fixed dock instead of Brick Play's tabs: one row of the 8 colours of
`Sails.Editor.palette` and one strip of the parts of `Sails.Editor.parts`.

The ship starts from one of the starter hulls (`Sails.Editor.bases`): bow,
deck and keel grouped as one submodel, so the hull is selected and moved as
one piece. The player adds parts (each goes on free studs of the deck, a sail
on the selected mast, `Sails.Editor.dropMatrix`), colours them, turns, raises,
lowers and drags them, and deletes them. The hull itself cannot be deleted,
recoloured or ungrouped: every studio update is checked with
`Sails.Editor.baseIntact` and one that would split the hull is dropped. Brick
Sails has no ungroup flow, so the hull stays fixed.

"Use ship" hands the document to `Main`, which loads it like a build link
(`validateShip`, "Your ship" in the shipyard, share link, race).

-}

import Dict exposing (Dict)
import FeatherIcons
import Game.Ports as GamePorts
import Html exposing (Html, button, div, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import I18n exposing (Language(..))
import LDraw.ColorData as ColorData
import LDraw.Colors as Colors
import LDraw.Document exposing (Document)
import Ports
import Sails.Editor as Editor exposing (Base, PartEntry)
import Set
import Studio.Colours as Colours
import Studio.Core as Core
import Studio.Shortcuts exposing (TurnDirection(..))
import UI.PartCard as PartCard
import Viewport


{-| Editor state: the studio, the hull it stands on, the colour for new parts
and the part pictures received so far (by `PartCard.thumbnailKey`).
-}
type alias Model =
    { studio : Core.Model
    , base : Base
    , colour : Int
    , language : Language
    , width : Int
    , height : Int
    , thumbnails : Dict String String
    , notice : Maybe String
    }


{-| Editor messages.
-}
type Msg
    = StudioMsg Core.Msg
    | PickColour Int
    | AddPart String
    | Turn TurnDirection
    | Raise
    | Lower
    | DeleteSelected
    | Undo
    | Redo
    | FitView
    | Finish
    | Leave
    | ThumbnailLoaded { file : String, color : Int, url : String }


{-| What the editor asks `Main` to do after an update.
-}
type Outcome
    = Stay
    | Quit
    | Finished Document


{-| Open the editor on a build that stands on the hull `base` (a starter document, or a
build made on it earlier).
-}
init :
    { ldrawBase : String, ldrawFallbackBase : String, supportsPointerEvents : Bool, language : Language, width : Int, height : Int }
    -> Base
    -> Document
    -> ( Model, Cmd Msg )
init config base doc =
    let
        ( studio, studioCmd ) =
            Core.init
                { ldrawBase = config.ldrawBase
                , ldrawFallbackBase = config.ldrawFallbackBase
                , defaultModelUrl = ""
                , initialHash = ""
                , supportsPointerEvents = config.supportsPointerEvents
                , document = doc
                , gridStep = Just 20
                , rotateStep = Just 90
                , snapDistance = Just 24.0
                , collisionMode = Nothing
                }
                |> Tuple.mapFirst
                    (Core.setGizmoStyle Core.NoGizmo
                        -- As in Brick Play: one finger (or a left drag) on a part
                        -- moves it, on empty space pans; two fingers or a right drag orbit.
                        >> Core.setDragMode (Core.DragPartsOrPan Core.OneFingerPans)
                        >> Core.setTwoFingerOrbits True
                        >> Core.setGlueEditable False
                        >> Core.setPasteEnabled False
                        >> Core.setNudgeSteps { horizontal = 20, vertical = 8 }
                    )

        model =
            { studio = studio
            , base = base
            , colour = Editor.defaultColour
            , language = config.language
            , width = config.width
            , height = config.height
            , thumbnails = Dict.empty
            , notice = Nothing
            }

        ( sized, sizeCmd ) =
            resize config.width config.height model
    in
    ( sized
    , Cmd.batch [ Cmd.map StudioMsg studioCmd, sizeCmd, requestThumbnails sized ]
    )


{-| The build as it stands.
-}
document : Model -> Document
document model =
    Core.getDocument model.studio


{-| Follow a change of the window size.
-}
resize : Int -> Int -> Model -> ( Model, Cmd Msg )
resize width height model =
    let
        sized =
            { model | width = width, height = height }

        ( next, cmd ) =
            runCore (Core.update (Core.ViewportMsg (Viewport.WindowResize width (canvasHeight sized)))) sized
    in
    ( next, cmd )


{-| Follow a change of the language.
-}
setLanguage : Language -> Model -> Model
setLanguage language model =
    { model | language = language }


{-| Handle an editor message.
-}
update : Msg -> Model -> ( Model, Cmd Msg, Outcome )
update msg model =
    let
        stay ( m, c ) =
            ( m, c, Stay )
    in
    case msg of
        StudioMsg studioMsg ->
            stay (runCore (Core.update studioMsg) model)

        PickColour code ->
            let
                picked =
                    { model | colour = code, notice = Nothing }
            in
            if Core.hasSelection model.studio && not (baseSelected model) then
                let
                    ( next, cmd ) =
                        runCore (Core.recolorSelection code) picked
                in
                ( next, Cmd.batch [ cmd, requestThumbnails next ], Stay )

            else if baseSelected model then
                ( { picked | notice = Just (keepBaseColours model.language) }, requestThumbnails picked, Stay )

            else
                ( picked, requestThumbnails picked, Stay )

        AddPart file ->
            let
                matrix =
                    Editor.dropMatrix (Core.getDocument model.studio) model.studio.selection file
            in
            stay (runCore (Core.addPartAt file model.colour matrix) { model | notice = Nothing })

        Turn direction ->
            stay (runCore (Core.turnSelected direction) model)

        Raise ->
            stay (runCore Core.raiseSelected model)

        Lower ->
            stay (runCore Core.lowerSelected model)

        DeleteSelected ->
            if baseSelected model then
                ( { model | notice = Just (keepBaseText model.language) }, Cmd.none, Stay )

            else
                stay (runCore Core.deleteSelection model)

        Undo ->
            stay (runCore Core.undo model)

        Redo ->
            stay (runCore Core.redo model)

        FitView ->
            let
                ( studio, cmd ) =
                    Core.fitView model.studio
            in
            ( { model | studio = studio }, Cmd.map StudioMsg cmd, Stay )

        Finish ->
            ( model, Cmd.none, Finished (Core.getDocument model.studio) )

        Leave ->
            ( model, Cmd.none, Quit )

        ThumbnailLoaded thumb ->
            ( { model | thumbnails = Dict.insert (PartCard.thumbnailKey thumb.file thumb.color) thumb.url model.thumbnails }
            , Cmd.none
            , Stay
            )


{-| Run a studio step, unless it would split the hull: then the step is
dropped (its model, commands and events) and the player is told why. This
catches every path to the hull: the keyboard (Delete, Ctrl+X, Ctrl+D,
Ctrl+G, Ctrl+Shift+G), the buttons and drags.
-}
runCore : (Core.Model -> ( Core.Model, Cmd Core.Msg, List Core.Event )) -> Model -> ( Model, Cmd Msg )
runCore step model =
    let
        ( studio, cmd, events ) =
            step model.studio
    in
    if Editor.baseIntact model.base (Core.getDocument studio) then
        ( { model | studio = studio }
        , Cmd.batch (Cmd.map StudioMsg cmd :: List.map eventCmd events)
        )

    else
        ( { model | notice = Just (keepBaseText model.language) }, Cmd.none )


eventCmd : Core.Event -> Cmd Msg
eventCmd event =
    case event of
        Core.FlattenRequested payload ->
            GamePorts.requestGeometryFlatten payload

        _ ->
            Cmd.none


baseSelected : Model -> Bool
baseSelected model =
    Core.getPlacements model.studio
        |> List.any (\p -> Set.member p.id model.studio.selection && Editor.isBasePlacement model.base p)


requestThumbnails : Model -> Cmd Msg
requestThumbnails model =
    let
        missing =
            Editor.parts
                |> List.filter (\p -> not (Dict.member (PartCard.thumbnailKey p.file model.colour) model.thumbnails))
    in
    if List.isEmpty missing then
        Cmd.none

    else
        Ports.requestThumbnails
            (List.map
                (\p ->
                    { file = p.file
                    , color = model.colour
                    , rgb = Colors.resolveColor model.colour 16 |> (\c -> [ c.r, c.g, c.b ])
                    }
                )
                missing
            )


{-| The studio's own subscriptions, the geometry worker and the part pictures.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map StudioMsg (Core.subscriptions model.studio)
        , GamePorts.geometryFlattened (StudioMsg << Core.geometryFlattened)
        , GamePorts.geometryFlattenFailed (StudioMsg << Core.geometryFlattenFailed)
        , Ports.thumbnailLoaded ThumbnailLoaded
        ]



-- ── View ─────────────────────────────────────────────────────────────────────


{-| Height of the dock at the bottom: the colour row and the parts strip.
-}
dockHeight : Int
dockHeight =
    132


canvasHeight : Model -> Int
canvasHeight model =
    max 160 (model.height - dockHeight)


{-| The editor screen: the studio canvas, a top bar, the selection tools and
the dock.
-}
view : Model -> Html Msg
view model =
    div
        [ Attr.class "brick-editor"
        , Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "overflow" "hidden"
        , Attr.style "background" "linear-gradient(180deg, #bfe3f5 0%, #e8f4ec 100%)"
        , Attr.style "color" "#f8fafc"
        , Attr.style "font-family" "var(--font-sans, system-ui, sans-serif)"
        , Attr.style "user-select" "none"
        , Attr.style "-webkit-user-select" "none"
        ]
        [ div
            [ Attr.style "position" "absolute"
            , Attr.style "top" "0"
            , Attr.style "left" "0"
            , Attr.style "width" (px model.width)
            , Attr.style "height" (px (canvasHeight model))
            , Attr.style "touch-action" "none"
            ]
            [ Html.map StudioMsg (Core.view { width = model.width, height = canvasHeight model } model.studio) ]
        , topBar model
        , selectionBar model
        , noticeView model
        , dock model
        ]


topBar : Model -> Html Msg
topBar model =
    let
        lang =
            model.language
    in
    div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "0"
        , Attr.style "left" "0"
        , Attr.style "right" "0"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "6px"
        , Attr.style "padding" "max(8px, env(safe-area-inset-top)) max(8px, env(safe-area-inset-right)) 8px max(8px, env(safe-area-inset-left))"
        , Attr.style "pointer-events" "none"
        ]
        [ iconButton (tr lang "Back to the boats" "Takaisin veneisiin") FeatherIcons.arrowLeft (Just Leave)
        , div
            [ Attr.style "flex" "1"
            , Attr.style "min-width" "0"
            , Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "padding" "4px 10px"
            , Attr.style "border-radius" "12px"
            , Attr.style "background" "rgba(15, 23, 42, 0.78)"
            , Attr.style "pointer-events" "auto"
            ]
            [ span [ Attr.style "font-size" "11px", Attr.style "color" "#94a3b8", Attr.style "white-space" "nowrap", Attr.style "overflow" "hidden", Attr.style "text-overflow" "ellipsis" ]
                [ text (tr lang "Build your ship" "Rakenna laivasi") ]
            , span
                [ Attr.style "font-size" "14px"
                , Attr.style "font-weight" "700"
                , Attr.style "white-space" "nowrap"
                , Attr.style "overflow" "hidden"
                , Attr.style "text-overflow" "ellipsis"
                ]
                [ text (baseName lang model.base) ]
            ]
        , iconButton (tr lang "Undo" "Kumoa") FeatherIcons.cornerUpLeft (enabledIf (Core.canUndo model.studio) Undo)
        , iconButton (tr lang "Redo" "Tee uudelleen") FeatherIcons.cornerUpRight (enabledIf (Core.canRedo model.studio) Redo)
        , iconButton (tr lang "Show the whole ship" "Näytä koko laiva") FeatherIcons.maximize (Just FitView)
        , button
            (buttonStyle True
                ++ [ onClick Finish
                   , Attr.style "background" "#facc15"
                   , Attr.style "color" "#0f172a"
                   , Attr.style "padding" "0 14px"
                   , Attr.style "font-weight" "800"
                   , Attr.style "gap" "6px"
                   , Attr.attribute "aria-label" (tr lang "Use this ship" "Käytä tätä laivaa")
                   ]
            )
            [ FeatherIcons.check |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml []
            , span [] [ text (tr lang "Use ship" "Valmis") ]
            ]
        ]


selectionBar : Model -> Html Msg
selectionBar model =
    let
        lang =
            model.language

        onBase =
            baseSelected model
    in
    if Core.hasSelection model.studio then
        div
            [ Attr.style "position" "absolute"
            , Attr.style "left" "50%"
            , Attr.style "transform" "translateX(-50%)"
            , Attr.style "bottom" (px (dockHeight + 10))
            , Attr.style "display" "flex"
            , Attr.style "gap" "6px"
            , Attr.style "padding" "6px"
            , Attr.style "border-radius" "14px"
            , Attr.style "background" "rgba(15, 23, 42, 0.82)"
            ]
            [ iconButton (tr lang "Turn left" "Käännä vasemmalle") FeatherIcons.rotateCcw (Just (Turn TurnLeft))
            , iconButton (tr lang "Turn right" "Käännä oikealle") FeatherIcons.rotateCw (Just (Turn TurnRight))
            , iconButton (tr lang "Raise" "Nosta") FeatherIcons.arrowUp (Just Raise)
            , iconButton (tr lang "Lower" "Laske") FeatherIcons.arrowDown (Just Lower)
            , iconButton
                (if onBase then
                    tr lang "The hull cannot be deleted" "Runkoa ei voi poistaa"

                 else
                    tr lang "Delete" "Poista"
                )
                FeatherIcons.trash2
                (enabledIf (not onBase) DeleteSelected)
            ]

    else
        text ""


noticeView : Model -> Html Msg
noticeView model =
    case model.notice of
        Just message ->
            div
                [ Attr.attribute "role" "status"
                , Attr.style "position" "absolute"
                , Attr.style "left" "50%"
                , Attr.style "transform" "translateX(-50%)"
                , Attr.style "top" "64px"
                , Attr.style "max-width" "calc(100% - 32px)"
                , Attr.style "padding" "6px 12px"
                , Attr.style "border-radius" "10px"
                , Attr.style "background" "rgba(15, 23, 42, 0.86)"
                , Attr.style "font-size" "13px"
                , Attr.style "text-align" "center"
                ]
                [ text message ]

        Nothing ->
            text ""


dock : Model -> Html Msg
dock model =
    div
        [ Attr.style "position" "absolute"
        , Attr.style "left" "0"
        , Attr.style "right" "0"
        , Attr.style "bottom" "0"
        , Attr.style "height" (px dockHeight)
        , Attr.style "box-sizing" "border-box"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "6px"
        , Attr.style "padding" "8px max(8px, env(safe-area-inset-right)) max(8px, env(safe-area-inset-bottom)) max(8px, env(safe-area-inset-left))"
        , Attr.style "background" "rgba(15, 23, 42, 0.94)"
        , Attr.style "border-top" "1px solid rgba(148, 163, 184, 0.25)"
        ]
        [ colourRow model, partsStrip model ]


colourRow : Model -> Html Msg
colourRow model =
    div
        [ Attr.attribute "role" "listbox"
        , Attr.attribute "aria-label" (tr model.language "Colour" "Väri")
        , Attr.style "display" "flex"
        , Attr.style "justify-content" "center"
        , Attr.style "gap" "6px"
        ]
        (List.map (swatchView model) Editor.palette)


swatchView : Model -> Editor.Swatch -> Html Msg
swatchView model swatch =
    let
        selected =
            swatch.code == model.colour

        name =
            tr model.language swatch.nameEn swatch.nameFi

        cssColour =
            Dict.get swatch.code ColorData.colorTable
                |> Maybe.map Colours.css
                |> Maybe.withDefault "#888"
    in
    button
        [ Attr.type_ "button"
        , onClick (PickColour swatch.code)
        , Attr.attribute "role" "option"
        , Attr.attribute "aria-selected"
            (if selected then
                "true"

             else
                "false"
            )
        , Attr.attribute "aria-label" name
        , Attr.title name
        , Attr.style "width" "34px"
        , Attr.style "height" "34px"
        , Attr.style "flex" "0 0 34px"
        , Attr.style "border-radius" "50%"
        , Attr.style "background" cssColour
        , Attr.style "cursor" "pointer"
        , Attr.style "touch-action" "manipulation"
        , Attr.style "border"
            (if selected then
                "3px solid #facc15"

             else
                "2px solid rgba(255, 255, 255, 0.35)"
            )
        , Attr.style "box-shadow"
            (if selected then
                "0 0 0 2px #0f172a inset"

             else
                "none"
            )
        ]
        []


partsStrip : Model -> Html Msg
partsStrip model =
    div
        [ Attr.attribute "aria-label" (tr model.language "Parts" "Osat")
        , Attr.class "palette-scroll-x"
        , Attr.style "display" "flex"
        , Attr.style "gap" "6px"
        , Attr.style "overflow-x" "auto"
        , Attr.style "overflow-y" "hidden"
        , Attr.style "scrollbar-width" "thin"
        , Attr.style "-webkit-overflow-scrolling" "touch"
        , Attr.style "padding-bottom" "2px"
        ]
        (List.map (partCard model) Editor.parts)


partCard : Model -> PartEntry -> Html Msg
partCard model entry =
    let
        name =
            tr model.language entry.nameEn entry.nameFi

        number =
            String.replace ".dat" "" entry.file
    in
    PartCard.compactView
        { imageUrl = Dict.get (PartCard.thumbnailKey entry.file model.colour) model.thumbnails
        , tooltip = name ++ " (" ++ number ++ ")"
        , ariaLabel = tr model.language "Add " "Lisää " ++ name
        , badge = Nothing
        , onClick = Just (AddPart entry.file)
        , attributes = []
        }


iconButton : String -> FeatherIcons.Icon -> Maybe Msg -> Html Msg
iconButton label icon maybeMsg =
    button
        (buttonStyle (maybeMsg /= Nothing)
            ++ [ Attr.attribute "aria-label" label
               , Attr.title label
               , Attr.disabled (maybeMsg == Nothing)
               ]
            ++ (case maybeMsg of
                    Just msg ->
                        [ onClick msg ]

                    Nothing ->
                        []
               )
        )
        [ icon |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [] ]


buttonStyle : Bool -> List (Html.Attribute Msg)
buttonStyle enabled =
    [ Attr.type_ "button"
    , Attr.style "pointer-events" "auto"
    , Attr.style "display" "inline-flex"
    , Attr.style "align-items" "center"
    , Attr.style "justify-content" "center"
    , Attr.style "min-width" "40px"
    , Attr.style "height" "40px"
    , Attr.style "flex-shrink" "0"
    , Attr.style "border-radius" "12px"
    , Attr.style "border" "1px solid rgba(148, 163, 184, 0.35)"
    , Attr.style "background" "rgba(15, 23, 42, 0.82)"
    , Attr.style "color" "#f8fafc"
    , Attr.style "cursor"
        (if enabled then
            "pointer"

         else
            "default"
        )
    , Attr.style "opacity"
        (if enabled then
            "1"

         else
            "0.4"
        )
    , Attr.style "touch-action" "manipulation"
    ]


enabledIf : Bool -> Msg -> Maybe Msg
enabledIf condition msg =
    if condition then
        Just msg

    else
        Nothing


px : Int -> String
px n =
    String.fromInt n ++ "px"


{-| The hull's name in the player's language.
-}
baseName : Language -> Base -> String
baseName lang base =
    tr lang base.nameEn base.nameFi


keepBaseText : Language -> String
keepBaseText lang =
    tr lang "The hull stays in one piece." "Runko pysyy yhtenä kappaleena."


keepBaseColours : Language -> String
keepBaseColours lang =
    tr lang "The hull keeps its colours. Pick a part you added to colour it." "Runko pitää värinsä. Valitse lisäämäsi osa värittääksesi sen."


tr : Language -> String -> String -> String
tr lang en fi =
    case lang of
        Fi ->
            fi

        En ->
            en
