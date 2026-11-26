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
fixed dock instead of Brick Play's tabs: a modelling toolbar (undo, redo,
turn, raise, lower, copy, paste, delete) above the palette row: the 8
colours of `Sails.Editor.palette` in two rows of four, the full-catalog
button and the parts used last, the active one starred. The full catalog
(`Sails.Editor.partCategories`, with a filter by name or number) opens
above the palette row; its button closes it again.
The top bar holds only icons: "Use ship" (the check mark) on the left, then
the hull's name with the hulls button beside it, "Show the whole ship" and
"Close". As in Brick Racer's bases drawer, the hulls button opens the
starter hulls above the palette row; picking another one starts the ship
again on that hull.

The ship starts from one of the starter hulls (`Sails.Editor.bases`): bow,
deck and keel grouped as one submodel, so the hull is selected and moved as
one piece. The player adds parts (each goes on free studs of the deck, a sail
on the selected mast, `Sails.Editor.dropMatrix`), colours them, turns, raises,
lowers, drags, copies and pastes them, and deletes them. The hull itself cannot be deleted,
recoloured or ungrouped: every studio update is checked with
`Sails.Editor.baseIntact` and one that would split the hull is dropped. Brick
Sails has no ungroup flow, so the hull stays fixed.

"Use ship" hands the document to `Main`, which loads it like a build link
(`validateShip`, "Your ship" in the shipyard, share link, race).

-}

import DesignTokens.Colors as Brand
import Dict exposing (Dict)
import FeatherIcons
import Game.Ports as GamePorts
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput, onMouseLeave)
import I18n exposing (Language(..))
import Json.Decode as Decode
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


{-| Editor state: the studio, the hull it stands on, the colour for new parts,
the part pictures received so far (by `PartCard.thumbnailKey`), whether the
full catalog is open and its filter, whether the hulls drawer is open, and
the palette row: the part added last (`activePart`) and the parts used
before it (`recentParts`).
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
    , catalogOpen : Bool
    , hullsOpen : Bool
    , catalogFilter : String
    , activePart : String
    , recentParts : List String
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
    | CopySelected
    | PasteSelected
    | Undo
    | Redo
    | FitView
    | Finish
    | Leave
    | ThumbnailLoaded { file : String, color : Int, url : String }
    | ToggleCatalog
    | ToggleHulls
    | SelectHull Base
    | CatalogFilterChanged String
    | PaletteLeft
    | CanvasTapped


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
            , catalogOpen = False
            , hullsOpen = False
            , catalogFilter = ""
            , activePart = "3001.dat"
            , recentParts = initialRecentParts
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


{-| The parts of the palette row before the player adds any: a hull brick, a
plate, a slope, a deck tile, a mast, a sail and a cannon.
-}
initialRecentParts : List String
initialRecentParts =
    [ "3001.dat", "3020.dat", "3039.dat", "3068b.dat", "3957a.dat", "2335.dat", "32828.dat" ]


{-| How many parts the palette row shows.
-}
paletteSize : Int
paletteSize =
    7


{-| Move the active part to the front of the palette row. The row keeps its
order while the player taps the same card again, and this runs once the
player has moved on.
-}
commitRecent : Model -> Model
commitRecent model =
    { model
        | recentParts =
            (model.activePart :: List.filter (\f -> f /= model.activePart) model.recentParts)
                |> List.take paletteSize
    }


{-| Messages that mean the player has moved on from the palette card they
tapped.
-}
endsPaletteTap : Msg -> Bool
endsPaletteTap msg =
    case msg of
        ToggleCatalog ->
            True

        ToggleHulls ->
            True

        SelectHull _ ->
            True

        PaletteLeft ->
            True

        CanvasTapped ->
            True

        PickColour _ ->
            True

        Turn _ ->
            True

        Raise ->
            True

        Lower ->
            True

        DeleteSelected ->
            True

        CopySelected ->
            True

        PasteSelected ->
            True

        Undo ->
            True

        Redo ->
            True

        FitView ->
            True

        _ ->
            False


{-| Handle an editor message. Opening or closing the full catalog or the
hulls drawer changes the height of the 3D view, so the studio is resized
whenever it does.
-}
update : Msg -> Model -> ( Model, Cmd Msg, Outcome )
update msg model =
    let
        shouldCommit =
            case msg of
                AddPart file ->
                    file /= model.activePart

                _ ->
                    endsPaletteTap msg

        ( next, cmd, outcome ) =
            updateModel msg
                (if shouldCommit then
                    commitRecent model

                 else
                    model
                )
    in
    if next.catalogOpen /= model.catalogOpen || next.hullsOpen /= model.hullsOpen then
        let
            ( resized, resizeCmd ) =
                runCore (Core.update (Core.ViewportMsg (Viewport.WindowResize next.width (canvasHeight next)))) next
        in
        ( resized, Cmd.batch [ cmd, resizeCmd ], outcome )

    else
        ( next, cmd, outcome )


updateModel : Msg -> Model -> ( Model, Cmd Msg, Outcome )
updateModel msg model =
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
            stay (runCore (Core.addPartAt file model.colour matrix) { model | notice = Nothing, activePart = file, catalogOpen = False })

        Turn direction ->
            stay (runCore (Core.turnSelected direction) model)

        Raise ->
            stay (runCore Core.raiseSelected model)

        Lower ->
            stay (runCore Core.lowerSelected model)

        CopySelected ->
            stay (runCore (\studio -> ( Core.copySelection studio, Cmd.none, [] )) model)

        PasteSelected ->
            stay (runCore Core.pasteClipboard model)

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

        ToggleCatalog ->
            ( { model | catalogOpen = not model.catalogOpen, hullsOpen = False, notice = Nothing }, Cmd.none, Stay )

        ToggleHulls ->
            ( { model | hullsOpen = not model.hullsOpen, catalogOpen = False, notice = Nothing }, Cmd.none, Stay )

        SelectHull base ->
            if base.id == model.base.id then
                ( { model | hullsOpen = False, notice = Nothing }, Cmd.none, Stay )

            else
                -- As Brick Racer's bases drawer: start again on the picked hull.
                let
                    ( resetStudio, resetCmd, events ) =
                        Core.resetDocument base.document model.studio

                    ( fittedStudio, fitCmd ) =
                        Core.fitView resetStudio
                in
                ( { model | studio = fittedStudio, base = base, hullsOpen = False, notice = Nothing }
                , Cmd.batch
                    (Cmd.map StudioMsg resetCmd
                        :: Cmd.map StudioMsg fitCmd
                        :: List.map eventCmd events
                    )
                , Stay
                )

        CatalogFilterChanged query ->
            ( { model | catalogFilter = query }, Cmd.none, Stay )

        PaletteLeft ->
            ( model, Cmd.none, Stay )

        CanvasTapped ->
            ( model, Cmd.none, Stay )


{-| Run a studio step, unless it would split the hull: then the step is
dropped (its model, commands and events) and the player is told why. This
catches every path to the hull: the keyboard (Delete, Ctrl+X, Ctrl+D,
Ctrl+V, Ctrl+G, Ctrl+Shift+G), the buttons and drags.
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


{-| Height of the dock at the bottom: the modelling toolbar and the palette
row (without the full catalog): the 44 px tools, the palette cards and the
gaps and padding around them.
-}
dockHeight : Int
dockHeight =
    144


{-| Height of the full catalog, shown above the palette row while open. On a
desktop-width screen it grows to nearly the full window; on a phone it is up
to twice its compact height. Either way a strip of the 3D view stays above it.
-}
catalogHeight : Model -> Int
catalogHeight model =
    let
        compact =
            240

        tallest =
            if model.width >= 768 then
                model.height

            else
                2 * compact
    in
    max 120 (min tallest (model.height - dockHeight - 120))


{-| Height of the hulls drawer, shown above the palette row while open: room
for the hull cards, never taller than the catalog.
-}
hullsHeight : Model -> Int
hullsHeight model =
    min 220 (catalogHeight model)


{-| Height of the whole bottom panel: the dock, and the catalog or the hulls
drawer while open.
-}
panelHeight : Model -> Int
panelHeight model =
    if model.catalogOpen then
        dockHeight + 6 + catalogHeight model

    else if model.hullsOpen then
        dockHeight + 6 + hullsHeight model

    else
        dockHeight


canvasHeight : Model -> Int
canvasHeight model =
    max 160 (model.height - panelHeight model)


{-| The editor screen: the studio canvas, a top bar and the dock.
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
            , Html.Events.on "pointerdown" (Decode.succeed CanvasTapped)
            ]
            [ Html.map StudioMsg (Core.view { width = model.width, height = canvasHeight model } model.studio) ]
        , topBar model
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
        , Attr.style "z-index" "30"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "6px"
        , Attr.style "padding" "max(8px, env(safe-area-inset-top)) max(8px, env(safe-area-inset-right)) 8px max(8px, env(safe-area-inset-left))"
        , Attr.style "pointer-events" "none"
        ]
        [ button
            (buttonStyle True
                ++ [ onClick Finish
                   , Html.Events.on "pointerdown" (Decode.succeed Finish)
                   , Attr.style "background" Brand.legoYellow
                   , Attr.style "color" Brand.legoBlack
                   , Attr.style "border" "none"
                   , Attr.attribute "aria-label" (tr lang "Use this ship" "Käytä tätä laivaa")
                   , Attr.title (tr lang "Use this ship" "Käytä tätä laivaa")
                   ]
            )
            [ FeatherIcons.check |> FeatherIcons.withSize 20 |> FeatherIcons.toHtml [] ]
        , div
            [ Attr.style "flex" "1"
            , Attr.style "min-width" "0"
            , Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "center"
            , Attr.style "gap" "6px"
            ]
            [ div
                [ Attr.style "min-width" "0"
                , Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "align-items" "center"
                , Attr.style "padding" "4px 10px"
                , Attr.style "border-radius" "12px"
                , Attr.style "background" "rgba(15, 23, 42, 0.78)"
                , Attr.style "pointer-events" "auto"
                ]
                [ span [ Attr.style "font-size" "11px", Attr.style "color" "#94a3b8", Attr.style "white-space" "nowrap", Attr.style "overflow" "hidden", Attr.style "text-overflow" "ellipsis", Attr.style "max-width" "100%" ]
                    [ text (tr lang "Build your ship" "Rakenna laivasi") ]
                , span
                    [ Attr.style "font-size" "14px"
                    , Attr.style "font-weight" "700"
                    , Attr.style "white-space" "nowrap"
                    , Attr.style "overflow" "hidden"
                    , Attr.style "text-overflow" "ellipsis"
                    , Attr.style "max-width" "100%"
                    ]
                    [ text (baseName lang model.base) ]
                ]
            , hullsButton model
            ]
        , iconButton (tr lang "Show the whole ship" "Näytä koko laiva") FeatherIcons.maximize (Just FitView)
        , iconButton (tr lang "Close without using the ship" "Sulje käyttämättä laivaa") FeatherIcons.x (Just Leave)
        ]


{-| Opens the hulls drawer above the palette row, and closes it again; it is
yellow while the drawer is open, like Brick Racer's drawer tabs.
-}
hullsButton : Model -> Html Msg
hullsButton model =
    let
        label =
            tr model.language "Change hull" "Vaihda runko"
    in
    button
        [ Attr.type_ "button"
        , onClick ToggleHulls
        , Html.Events.on "pointerdown" (Decode.succeed ToggleHulls)
        , Attr.attribute "data-hulls-toggle" ""
        , Attr.attribute "aria-label" label
        , Attr.attribute "aria-pressed"
            (if model.hullsOpen then
                "true"

             else
                "false"
            )
        , Attr.title label
        , Attr.style "display" "inline-flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "width" "40px"
        , Attr.style "height" "40px"
        , Attr.style "flex-shrink" "0"
        , Attr.style "border-radius" "12px"
        , Attr.style "border"
            (if model.hullsOpen then
                "2px solid " ++ Brand.legoYellow

             else
                "1px solid rgba(255, 255, 255, 0.18)"
            )
        , Attr.style "background"
            (if model.hullsOpen then
                Brand.legoYellow

             else
                "rgba(15, 23, 42, 0.82)"
            )
        , Attr.style "color"
            (if model.hullsOpen then
                Brand.legoBlack

             else
                "#e2e8f0"
            )
        , Attr.style "cursor" "pointer"
        , Attr.style "pointer-events" "auto"
        , Attr.style "touch-action" "manipulation"
        , Attr.style "transition" "all 0.15s ease"
        ]
        [ FeatherIcons.anchor |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [] ]


{-| The modelling toolbar above the palette: undo and redo, then the tools
for the selected part. A tool is greyed out while it has nothing to work on.
Copy and paste appear from 640 px wide, so they stay off a narrow phone in
portrait; Ctrl+C, Ctrl+V and Ctrl+D work on a keyboard.
-}
modellingControls : Model -> Html Msg
modellingControls model =
    let
        lang =
            model.language

        hasSelection =
            Core.hasSelection model.studio

        onBase =
            baseSelected model

        wide =
            model.width >= 640
    in
    div
        [ Attr.attribute "role" "toolbar"
        , Attr.attribute "aria-label" (tr lang "Part tools" "Osan työkalut")
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "gap" "6px"
        , Attr.style "flex-shrink" "0"
        ]
        [ toolButton (tr lang "Undo" "Kumoa") FeatherIcons.cornerUpLeft (enabledIf (Core.canUndo model.studio) Undo)
        , toolButton (tr lang "Redo" "Tee uudelleen") FeatherIcons.cornerUpRight (enabledIf (Core.canRedo model.studio) Redo)
        , toolButton (tr lang "Turn left" "Käännä vasemmalle") FeatherIcons.rotateCcw (enabledIf hasSelection (Turn TurnLeft))
        , toolButton (tr lang "Turn right" "Käännä oikealle") FeatherIcons.rotateCw (enabledIf hasSelection (Turn TurnRight))
        , toolButton (tr lang "Raise" "Nosta") FeatherIcons.arrowUp (enabledIf hasSelection Raise)
        , toolButton (tr lang "Lower" "Laske") FeatherIcons.arrowDown (enabledIf hasSelection Lower)
        , if wide then
            toolButton (tr lang "Copy" "Kopioi") FeatherIcons.copy (enabledIf (hasSelection && not onBase) CopySelected)

          else
            text ""
        , if wide then
            toolButton (tr lang "Paste" "Liitä") FeatherIcons.clipboard (Just PasteSelected)

          else
            text ""
        , toolButton
            (if onBase then
                tr lang "The hull cannot be deleted" "Runkoa ei voi poistaa"

             else
                tr lang "Delete" "Poista"
            )
            FeatherIcons.trash2
            (enabledIf (hasSelection && not onBase) DeleteSelected)
        ]


{-| A 44 px touch target of the modelling toolbar.
-}
toolButton : String -> FeatherIcons.Icon -> Maybe Msg -> Html Msg
toolButton label icon maybeMsg =
    let
        enabled =
            maybeMsg /= Nothing
    in
    button
        ([ Attr.type_ "button"
         , Attr.attribute "aria-label" label
         , Attr.title label
         , Attr.disabled (not enabled)
         , Attr.style "display" "inline-flex"
         , Attr.style "align-items" "center"
         , Attr.style "justify-content" "center"
         , Attr.style "width" "44px"
         , Attr.style "height" "44px"
         , Attr.style "flex-shrink" "0"
         , Attr.style "border-radius" "10px"
         , Attr.style "border"
            (if enabled then
                "1px solid rgba(255, 255, 255, 0.25)"

             else
                "1px solid rgba(255, 255, 255, 0.08)"
            )
         , Attr.style "background"
            (if enabled then
                "rgba(30, 41, 59, 0.85)"

             else
                "rgba(15, 23, 42, 0.4)"
            )
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
                "0.35"
            )
         , Attr.style "touch-action" "manipulation"
         ]
            ++ (case maybeMsg of
                    Just msg ->
                        [ onClick msg
                        , Html.Events.on "pointerdown" (Decode.succeed msg)
                        ]

                    Nothing ->
                        []
               )
        )
        [ icon |> FeatherIcons.withSize 16 |> FeatherIcons.toHtml [] ]


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
        , Attr.style "height" ("calc(" ++ px (panelHeight model) ++ " + env(safe-area-inset-bottom, 0px))")
        , Attr.style "box-sizing" "border-box"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "6px"
        , Attr.style "padding" "6px max(8px, env(safe-area-inset-right)) max(6px, env(safe-area-inset-bottom)) max(8px, env(safe-area-inset-left))"
        , Attr.style "background" "rgba(15, 23, 42, 0.94)"
        , Attr.style "border-top" "1px solid rgba(148, 163, 184, 0.25)"
        ]
        ((if model.catalogOpen then
            [ catalogView model ]

          else if model.hullsOpen then
            [ hullsView model ]

          else
            []
         )
            ++ [ modellingControls model, paletteRow model ]
        )


colourRow : Model -> Html Msg
colourRow model =
    div
        [ Attr.attribute "role" "listbox"
        , Attr.attribute "aria-label" (tr model.language "Colour" "Väri")
        , Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "repeat(4, 26px)"
        , Attr.style "grid-template-rows" "repeat(2, 26px)"
        , Attr.style "gap" "4px"
        , Attr.style "flex-shrink" "0"
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
        , Attr.style "width" "26px"
        , Attr.style "height" "26px"
        , Attr.style "box-sizing" "border-box"
        , Attr.style "border-radius" "50%"
        , Attr.style "background" cssColour
        , Attr.style "cursor" "pointer"
        , Attr.style "touch-action" "manipulation"
        , Attr.style "border"
            (if selected then
                "2.5px solid " ++ Brand.legoYellow

             else
                "1.5px solid rgba(255, 255, 255, 0.4)"
            )
        , Attr.style "box-shadow"
            (if selected then
                "0 0 0 1.5px " ++ Brand.legoBlack ++ " inset"

             else
                "none"
            )
        ]
        []


{-| The palette row: the colours, then the full-catalog button and the parts
used last, centred while they fit and scrolling sideways when they do not.
-}
paletteRow : Model -> Html Msg
paletteRow model =
    let
        shownParts =
            (if List.member model.activePart model.recentParts then
                model.recentParts

             else
                model.activePart :: model.recentParts
            )
                |> List.take paletteSize
    in
    div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "gap" "8px"
        , Attr.style "width" "100%"
        , Attr.style "box-sizing" "border-box"
        , onMouseLeave PaletteLeft
        ]
        [ colourRow model
        , div
            [ Attr.style "width" "1px"
            , Attr.style "height" "48px"
            , Attr.style "background" "rgba(255, 255, 255, 0.18)"
            , Attr.style "flex-shrink" "0"
            ]
            []
        , div
            [ Attr.attribute "aria-label" (tr model.language "Parts" "Osat")
            , Attr.class "palette-scroll-x"
            , Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "flex" "1"
            , Attr.style "min-width" "0"
            , Attr.style "overflow-x" "auto"
            , Attr.style "overflow-y" "hidden"
            , Attr.style "scrollbar-width" "thin"
            , Attr.style "-webkit-overflow-scrolling" "touch"
            , Attr.style "padding-bottom" "2px"
            ]
            [ div
                [ Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "6px"
                , Attr.style "flex-shrink" "0"
                , Attr.style "margin" "0 auto"
                ]
                (catalogButton model :: List.filterMap (partCard model) shownParts)
            ]
        ]


{-| Opens the full catalog above the palette row, and closes it again.
-}
catalogButton : Model -> Html Msg
catalogButton model =
    button
        [ Attr.type_ "button"
        , onClick ToggleCatalog
        , Attr.attribute "data-catalog-toggle" ""
        , Attr.style "flex-shrink" "0"
        , Attr.style "width" "56px"
        , Attr.style "height" "56px"
        , Attr.style "padding" "0"
        , Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "border-radius" "10px"
        , Attr.style "border" "2px dashed rgba(250, 200, 10, 0.6)"
        , Attr.style "background"
            (if model.catalogOpen then
                "rgba(250, 200, 10, 0.35)"

             else
                "rgba(250, 200, 10, 0.15)"
            )
        , Attr.style "color" Brand.legoYellow
        , Attr.style "cursor" "pointer"
        , Attr.style "touch-action" "manipulation"
        , Attr.attribute "aria-label" (tr model.language "All parts" "Kaikki osat")
        , Attr.attribute "aria-pressed"
            (if model.catalogOpen then
                "true"

             else
                "false"
            )
        , Attr.title (tr model.language "Browse all parts" "Selaa kaikkia osia")
        ]
        [ FeatherIcons.box |> FeatherIcons.withSize 22 |> FeatherIcons.toHtml [] ]


{-| A part card of the palette row; the active part (added last) is starred.
-}
partCard : Model -> String -> Maybe (Html Msg)
partCard model file =
    Editor.parts
        |> List.filter (\p -> p.file == file)
        |> List.head
        |> Maybe.map
            (\entry ->
                let
                    isActive =
                        file == model.activePart

                    name =
                        tr model.language entry.nameEn entry.nameFi

                    number =
                        String.replace ".dat" "" entry.file
                in
                PartCard.compactView
                    { imageUrl = Dict.get (PartCard.thumbnailKey entry.file model.colour) model.thumbnails
                    , tooltip = name ++ " (" ++ number ++ ")"
                    , ariaLabel = tr model.language "Add " "Lisää " ++ name
                    , badge =
                        if isActive then
                            Just { text = "★", muted = False }

                        else
                            Nothing
                    , onClick = Just (AddPart entry.file)
                    , attributes =
                        if isActive then
                            [ Attr.style "border" ("2px solid " ++ Brand.legoYellow)
                            , Attr.style "box-shadow" "0 0 8px rgba(250, 200, 10, 0.4)"
                            ]

                        else
                            []
                    }
            )


{-| The full catalog: every part of the editor in its group, with a filter
field that keeps to the top. Groups with no matching part are hidden.
-}
catalogView : Model -> Html Msg
catalogView model =
    let
        lang =
            model.language

        matching category =
            List.filter (matchesFilter model.catalogFilter) category.parts

        section category =
            div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "8px" ]
                [ div
                    [ Attr.style "font-size" "12px"
                    , Attr.style "font-weight" "800"
                    , Attr.style "color" Brand.legoYellow
                    , Attr.style "text-transform" "uppercase"
                    , Attr.style "letter-spacing" "0.5px"
                    ]
                    [ text (tr lang category.titleEn category.titleFi) ]
                , div
                    [ Attr.style "display" "grid"
                    , Attr.style "grid-template-columns" "repeat(auto-fill, minmax(110px, 1fr))"
                    , Attr.style "gap" "8px"
                    ]
                    (List.map (catalogCard model) (matching category))
                ]
    in
    div
        [ Attr.attribute "data-catalog" ""
        , Attr.style "flex-shrink" "0"
        , Attr.style "height" (px (catalogHeight model))
        , Attr.style "box-sizing" "border-box"
        , Attr.style "overflow-y" "auto"
        , Attr.style "padding" "10px 12px"
        , Attr.style "border-radius" "12px"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.12)"
        , Attr.style "background" "rgba(5, 19, 29, 0.96)"
        ]
        [ div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "12px" ]
            [ div
                [ Attr.style "position" "sticky"
                , Attr.style "top" "0"
                , Attr.style "z-index" "1"
                , Attr.style "display" "flex"
                , Attr.style "justify-content" "center"
                , Attr.style "padding-bottom" "2px"
                , Attr.style "background" "rgba(5, 19, 29, 0.96)"
                ]
                [ input
                    [ Attr.type_ "search"
                    , Attr.value model.catalogFilter
                    , onInput CatalogFilterChanged
                    , Attr.placeholder (tr lang "Filter by name or number" "Suodata nimellä tai numerolla")
                    , Attr.attribute "aria-label" (tr lang "Filter parts" "Suodata osia")
                    , Attr.attribute "autocomplete" "off"
                    , Attr.attribute "autocapitalize" "none"
                    , Attr.attribute "spellcheck" "false"
                    , Attr.style "width" "100%"
                    , Attr.style "max-width" "280px"
                    , Attr.style "box-sizing" "border-box"
                    , Attr.style "padding" "8px 12px"
                    , Attr.style "border-radius" "10px"
                    , Attr.style "border" "1px solid rgba(255, 255, 255, 0.25)"
                    , Attr.style "background" "rgba(15, 23, 42, 0.85)"
                    , Attr.style "color" "#f8fafc"
                    , Attr.style "font-size" "14px"
                    ]
                    []
                ]
            , div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "16px" ]
                (Editor.partCategories
                    |> List.filter (\category -> not (List.isEmpty (matching category)))
                    |> List.map section
                )
            ]
        ]


{-| A part of the full catalog: its picture on the palette cards' light tile,
its name and its number.
-}
catalogCard : Model -> PartEntry -> Html Msg
catalogCard model entry =
    let
        name =
            tr model.language entry.nameEn entry.nameFi

        number =
            String.replace ".dat" "" entry.file
    in
    button
        [ Attr.type_ "button"
        , onClick (AddPart entry.file)
        , Attr.attribute "aria-label" (tr model.language "Add " "Lisää " ++ name)
        , Attr.title (name ++ " (" ++ number ++ ")")
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "gap" "4px"
        , Attr.style "padding" "8px 6px"
        , Attr.style "border-radius" "10px"
        , Attr.style "background" "rgba(15, 23, 42, 0.75)"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.12)"
        , Attr.style "color" "#f8fafc"
        , Attr.style "cursor" "pointer"
        , Attr.style "touch-action" "manipulation"
        ]
        [ div
            [ Attr.style "width" "64px"
            , Attr.style "height" "64px"
            , Attr.style "box-sizing" "border-box"
            , Attr.style "padding" "4px"
            , Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "center"
            , Attr.style "border-radius" "8px"
            , Attr.style "background"
                "radial-gradient(circle at 50% 45%, var(--color-bg-subtle) 0%, color-mix(in srgb, var(--color-border-default) 80%, var(--color-text-subtle)) 75%)"
            , Attr.style "box-shadow" "inset 0 0 0 1px rgba(255, 255, 255, 0.35)"
            ]
            [ case Dict.get (PartCard.thumbnailKey entry.file model.colour) model.thumbnails of
                Just url ->
                    Html.img
                        [ Attr.src url
                        , Attr.alt name
                        , Attr.style "max-width" "100%"
                        , Attr.style "max-height" "100%"
                        , Attr.style "object-fit" "contain"
                        ]
                        []

                Nothing ->
                    text ""
            ]
        , div [ Attr.style "font-size" "11px", Attr.style "font-weight" "700", Attr.style "text-align" "center", Attr.style "line-height" "1.2" ]
            [ text name ]
        , div [ Attr.style "font-size" "9px", Attr.style "color" "#94a3b8" ]
            [ text ("#" ++ number) ]
        ]


{-| The hulls drawer, as Brick Racer's bases drawer: a card for each starter
hull with its prerendered picture (`public/base-previews/<id>.png`, drawn
from the hull's real parts by `starters/tools/render_base_previews.mjs`), its
name and part count, the current one framed in yellow. Picking another hull
starts the ship again on it.
-}
hullsView : Model -> Html Msg
hullsView model =
    let
        lang =
            model.language

        hullCard base =
            let
                isCurrent =
                    base.id == model.base.id
            in
            button
                [ Attr.type_ "button"
                , onClick (SelectHull base)
                , Attr.attribute "data-hull" base.id
                , Attr.attribute "aria-pressed"
                    (if isCurrent then
                        "true"

                     else
                        "false"
                    )
                , Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "10px"
                , Attr.style "padding" "10px 12px"
                , Attr.style "border-radius" "12px"
                , Attr.style "border"
                    (if isCurrent then
                        "2px solid " ++ Brand.legoYellow

                     else
                        "1px solid rgba(255, 255, 255, 0.15)"
                    )
                , Attr.style "background"
                    (if isCurrent then
                        "rgba(250, 200, 10, 0.16)"

                     else
                        "rgba(15, 23, 42, 0.7)"
                    )
                , Attr.style "color" "#f8fafc"
                , Attr.style "cursor" "pointer"
                , Attr.style "text-align" "left"
                , Attr.style "touch-action" "manipulation"
                ]
                [ div
                    [ Attr.style "width" "72px"
                    , Attr.style "height" "72px"
                    , Attr.style "flex-shrink" "0"
                    , Attr.style "border-radius" "8px"
                    , Attr.style "overflow" "hidden"
                    , Attr.style "background" "#05131d"
                    , Attr.style "border" "1px solid rgba(255, 255, 255, 0.08)"
                    ]
                    [ Html.img
                        [ Attr.src ("base-previews/" ++ base.id ++ ".png")
                        , Attr.alt (baseName lang base)
                        , Attr.style "width" "100%"
                        , Attr.style "height" "100%"
                        , Attr.style "object-fit" "contain"
                        ]
                        []
                    ]
                , div [ Attr.style "min-width" "0" ]
                    [ div [ Attr.style "font-weight" "800", Attr.style "font-size" "13px", Attr.style "line-height" "1.2" ] [ text (baseName lang base) ]
                    , div [ Attr.style "font-size" "11px", Attr.style "color" "#94a3b8", Attr.style "margin-top" "2px" ]
                        [ text (String.fromInt base.partCount ++ tr lang " parts · bow, deck and hull" " osaa · keula, kansi ja runko") ]
                    ]
                ]
    in
    div
        [ Attr.attribute "data-hulls" ""
        , Attr.style "flex-shrink" "0"
        , Attr.style "height" (px (hullsHeight model))
        , Attr.style "box-sizing" "border-box"
        , Attr.style "overflow-y" "auto"
        , Attr.style "padding" "10px 12px"
        , Attr.style "border-radius" "12px"
        , Attr.style "border" "1px solid rgba(255, 255, 255, 0.12)"
        , Attr.style "background" "rgba(5, 19, 29, 0.96)"
        ]
        [ div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "10px" ]
            [ div
                [ Attr.style "font-size" "12px"
                , Attr.style "font-weight" "800"
                , Attr.style "color" Brand.legoYellow
                , Attr.style "text-transform" "uppercase"
                , Attr.style "letter-spacing" "0.5px"
                ]
                [ text (tr lang "Choose a starter hull" "Valitse aloitusrunko") ]
            , div
                [ Attr.style "display" "grid"
                , Attr.style "grid-template-columns" "repeat(auto-fill, minmax(200px, 1fr))"
                , Attr.style "gap" "10px"
                ]
                (List.map hullCard Editor.bases)
            ]
        ]


{-| Whether a part matches the catalog filter: by its English or Finnish
name, or by its number.
-}
matchesFilter : String -> PartEntry -> Bool
matchesFilter query entry =
    let
        q =
            String.toLower (String.trim query)
    in
    (q == "")
        || String.contains q (String.toLower entry.nameEn)
        || String.contains q (String.toLower entry.nameFi)
        || String.contains q (String.replace ".dat" "" entry.file)


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
                        [ onClick msg
                        , Html.Events.on "pointerdown" (Decode.succeed msg)
                        ]

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
