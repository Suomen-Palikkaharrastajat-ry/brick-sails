module Pages.Build exposing
    ( BuilderTab(..)
    , Model
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

{-| The ship editor (`Pages.Build`): a Brick Play builder for one ship, matching
Brick Racer's builder experience.

Includes the 3D builder `Studio.Core`, the parts dock with colour palette, recent
parts and base lock toggle, starter hulls drawer (`BasesTab`), build link / LDraw
import drawer (`ImportBuildTab`), and live seaworthiness & rigging evaluation
drawer (`ReviewTab`) with one-tap helpers and hints to add masts, sails, cannons,
and deck fittings.

The top bar holds only icons, matching Brick Racer:

  - Left: "Use this ship" (`FeatherIcons.check`)
  - Center: three drawer tabs — Starter hulls (`FeatherIcons.anchor`), Open build
    (`FeatherIcons.download`), and Evaluation (`FeatherIcons.award`)
  - Right: "Show the whole ship" (`FeatherIcons.maximize`)

-}

import DesignTokens.Colors as Brand
import Dict exposing (Dict)
import FeatherIcons
import Game.Ports as GamePorts
import Html exposing (Html, button, div, h3, input, span, text, textarea)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput, onMouseLeave)
import I18n exposing (Language(..))
import Json.Decode as Decode
import LDraw.ColorData as ColorData
import LDraw.Colors as Colors
import LDraw.Document as Document exposing (Document)
import Ports
import Sails.Analyze as Analyze
import Sails.Editor as Editor exposing (Base, PartEntry)
import Sails.Ingest as Ingest
import Set
import Studio.Colours as Colours
import Studio.Core as Core
import Studio.Shortcuts exposing (TurnDirection(..))
import UI.PartCard as PartCard
import Viewport


{-| Tabs available within the ship builder experience (matching Brick Racer).
-}
type BuilderTab
    = BuildBricksTab
    | AllBricksTab
    | BasesTab
    | ImportBuildTab
    | ReviewTab


{-| Editor state: the studio, the hull it stands on, whether the hull is locked,
the colour for new parts, the part pictures received so far (by
`PartCard.thumbnailKey`), the active drawer tab (`catalogOpen` and `hullsOpen`
stay in sync with `activeTab`), the catalog filter, paste buffer, and the
palette row (`activePart` and `recentParts`).
-}
type alias Model =
    { studio : Core.Model
    , base : Base
    , baseLocked : Bool
    , colour : Int
    , language : Language
    , width : Int
    , height : Int
    , thumbnails : Dict String String
    , notice : Maybe String
    , activeTab : BuilderTab
    , catalogOpen : Bool
    , hullsOpen : Bool
    , catalogFilter : String
    , pasteText : String
    , pasteError : Maybe String
    , activePart : String
    , recentParts : List String
    }


{-| Editor messages.
-}
type Msg
    = StudioMsg Core.Msg
    | PickColour Int
    | AddPart String
    | AddPartList (List String)
    | SelectPlacements (List Int)
    | RemovePlacements (List Int)
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
    | SelectTab BuilderTab
    | ToggleCatalog
    | ToggleHulls
    | SelectHull Base
    | ToggleBaseLock
    | PasteTextChanged String
    | SubmitPaste
    | CatalogFilterChanged String
    | PaletteLeft
    | CanvasTapped


{-| What the editor asks `Main` to do after an update.
-}
type Outcome
    = Stay
    | Quit
    | Finished Document


{-| Open the editor on a build that stands on the hull `base` (a starter
document, or a build made on it earlier).
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
                        -- As in Brick Play & Brick Racer: one finger (or a left drag) on a part
                        -- moves it, on empty space pans; two fingers or a right drag orbit.
                        >> Core.setDragMode (Core.DragPartsOrPan Core.OneFingerPans)
                        >> Core.setTwoFingerOrbits True
                        >> Core.setGlueEditable False
                        >> Core.setNudgeSteps { horizontal = 20, vertical = 8 }
                    )

        ( fittedStudio, fitCmd ) =
            Core.fitView studio

        model =
            { studio = fittedStudio
            , base = base
            , baseLocked = True
            , colour = Editor.defaultColour
            , language = config.language
            , width = config.width
            , height = config.height
            , thumbnails = Dict.empty
            , notice = Nothing
            , activeTab = BuildBricksTab
            , catalogOpen = False
            , hullsOpen = False
            , catalogFilter = ""
            , pasteText = ""
            , pasteError = Nothing
            , activePart = "3001.dat"
            , recentParts = initialRecentParts
            }

        ( sized, sizeCmd ) =
            resize config.width config.height model
    in
    ( sized
    , Cmd.batch [ Cmd.map StudioMsg studioCmd, Cmd.map StudioMsg fitCmd, sizeCmd, requestThumbnails sized ]
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


{-| Set the active drawer tab and keep `catalogOpen` and `hullsOpen` in sync.
-}
setTab : BuilderTab -> Model -> Model
setTab tab model =
    { model
        | activeTab = tab
        , catalogOpen = tab == AllBricksTab
        , hullsOpen = tab == BasesTab
        , notice = Nothing
    }


{-| Toggle a drawer tab: pressing the active tab's button closes it back to
`BuildBricksTab`.
-}
toggleTab : BuilderTab -> Model -> Model
toggleTab tab model =
    if model.activeTab == tab then
        setTab BuildBricksTab model

    else
        setTab tab model


{-| Messages that mean the player has moved on from the palette card they
tapped.
-}
endsPaletteTap : Msg -> Bool
endsPaletteTap msg =
    case msg of
        SelectTab _ ->
            True

        ToggleCatalog ->
            True

        ToggleHulls ->
            True

        SelectHull _ ->
            True

        ToggleBaseLock ->
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


{-| Handle an editor message. Any change of drawer tab changes the height of
the 3D view, so the studio is resized whenever it does.
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
    if next.activeTab /= model.activeTab || next.catalogOpen /= model.catalogOpen || next.hullsOpen /= model.hullsOpen then
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

            else if Core.hasSelection model.studio && baseSelected model then
                ( { picked | notice = Just (keepBaseColours model.language) }, requestThumbnails picked, Stay )

            else
                ( picked, requestThumbnails picked, Stay )

        AddPart file ->
            let
                matrix =
                    Editor.dropMatrix (Core.getDocument model.studio) model.studio.selection file

                partName =
                    Editor.parts
                        |> List.filter (\p -> p.file == file)
                        |> List.head
                        |> Maybe.map (\p -> tr model.language p.nameEn p.nameFi)
                        |> Maybe.withDefault file

                noticeText =
                    tr model.language ("➕ Added: " ++ partName) ("➕ Lisätty: " ++ partName)

                closed =
                    setTab BuildBricksTab model
            in
            stay
                (runCore
                    (Core.addPartAt file model.colour matrix)
                    { closed | activePart = file, notice = Just noticeText }
                )

        AddPartList files ->
            List.foldl
                (\file ( acc, accCmd, _ ) ->
                    let
                        ( added, addCmd, _ ) =
                            updateModel (AddPart file) acc
                    in
                    ( added, Cmd.batch [ accCmd, addCmd ], Stay )
                )
                ( model, Cmd.none, Stay )
                files

        SelectPlacements ids ->
            stay (selectPlacements ids (setTab BuildBricksTab model))

        RemovePlacements ids ->
            let
                doc =
                    Core.getDocument model.studio

                removable =
                    Document.mainPlacements doc
                        |> List.filter
                            (\p ->
                                List.member p.id ids
                                    && not (Document.isSubmodelReference p doc)
                                    && not (isLockedBase model p)
                            )
                        |> List.map .id

                kept =
                    List.filter (\id -> not (List.member id removable)) ids

                closed =
                    setTab BuildBricksTab model
            in
            if List.isEmpty removable then
                stay (selectPlacements ids closed)

            else
                let
                    ( selected, selectCmd ) =
                        runCore (Core.update (Core.Select (Set.fromList removable))) closed

                    ( removed, removeCmd ) =
                        runCore Core.deleteSelection selected

                    ( shown, showCmd ) =
                        if List.isEmpty kept then
                            ( removed, Cmd.none )

                        else
                            selectPlacements kept removed
                in
                ( shown, Cmd.batch [ selectCmd, removeCmd, showCmd ], Stay )

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

        SelectTab tab ->
            ( toggleTab tab model, Cmd.none, Stay )

        ToggleCatalog ->
            ( toggleTab AllBricksTab model, Cmd.none, Stay )

        ToggleHulls ->
            ( toggleTab BasesTab model, Cmd.none, Stay )

        SelectHull base ->
            if base.id == model.base.id then
                ( setTab BuildBricksTab model, Cmd.none, Stay )

            else
                let
                    ( resetStudio, resetCmd, events ) =
                        Core.resetDocument base.document model.studio

                    ( fittedStudio, fitCmd ) =
                        Core.fitView resetStudio

                    closed =
                        setTab BuildBricksTab model
                in
                ( { closed | studio = fittedStudio, base = base, baseLocked = True }
                , Cmd.batch
                    (Cmd.map StudioMsg resetCmd
                        :: Cmd.map StudioMsg fitCmd
                        :: List.map eventCmd events
                    )
                , Stay
                )

        ToggleBaseLock ->
            ( { model | baseLocked = not model.baseLocked, notice = Nothing }, Cmd.none, Stay )

        PasteTextChanged textVal ->
            ( { model | pasteText = textVal, pasteError = Nothing }, Cmd.none, Stay )

        SubmitPaste ->
            case Ingest.decodeShipInput Nothing model.pasteText of
                Ok decoded ->
                    let
                        newBase =
                            Editor.baseOf decoded.document |> Maybe.withDefault model.base

                        ( nextStudio, cmd, events ) =
                            Core.resetDocument decoded.document model.studio

                        ( fittedStudio, fitCmd ) =
                            Core.fitView nextStudio

                        closed =
                            setTab BuildBricksTab model

                        updated =
                            { closed
                                | studio = fittedStudio
                                , base = newBase
                                , baseLocked = Editor.baseOf decoded.document /= Nothing
                                , pasteText = ""
                                , pasteError = Nothing
                            }
                    in
                    ( updated
                    , Cmd.batch
                        [ Cmd.map StudioMsg cmd
                        , Cmd.map StudioMsg fitCmd
                        , Cmd.batch (List.map eventCmd events)
                        , requestThumbnails updated
                        ]
                    , Stay
                    )

                Err err ->
                    ( { model | pasteError = Just err }, Cmd.none, Stay )

        CatalogFilterChanged query ->
            ( { model | catalogFilter = query }, Cmd.none, Stay )

        PaletteLeft ->
            ( model, Cmd.none, Stay )

        CanvasTapped ->
            ( model, Cmd.none, Stay )


{-| Run a studio step, unless the hull is locked and the step would split it:
then the step is dropped (its model, commands and events) and the player is
told why.
-}
runCore : (Core.Model -> ( Core.Model, Cmd Core.Msg, List Core.Event )) -> Model -> ( Model, Cmd Msg )
runCore step model =
    let
        ( studio, cmd, events ) =
            step model.studio

        isAllowed =
            not model.baseLocked || Editor.baseIntact model.base (Core.getDocument studio)
    in
    if isAllowed then
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


{-| Select the main-model placements that hold the flattened placements `ids`.
-}
selectPlacements : List Int -> Model -> ( Model, Cmd Msg )
selectPlacements ids model =
    let
        doc =
            Core.getDocument model.studio

        holds p =
            List.member p.id ids
                || List.any (\inner -> List.member inner.id ids) (Document.constituentPlacements p.id doc)

        selection =
            Document.mainPlacements doc
                |> List.filter holds
                |> List.map .id
                |> Set.fromList
    in
    runCore (Core.update (Core.Select selection)) model


{-| Whether `placement` is the locked starter hull.
-}
isLockedBase : Model -> Document.Placement -> Bool
isLockedBase model placement =
    model.baseLocked && Editor.isBasePlacement model.base placement


baseSelected : Model -> Bool
baseSelected model =
    model.baseLocked
        && (Core.getPlacements model.studio
                |> List.any (\p -> Set.member p.id model.studio.selection && Editor.isBasePlacement model.base p)
           )


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
row (without the full catalog).
-}
dockHeight : Int
dockHeight =
    126


{-| Compact height of the full catalog drawer.
-}
catalogDrawerHeight : Int
catalogDrawerHeight =
    240


{-| Height of a drawer. On desktop-width screens it grows to nearly the full
window, leaving a strip of the 3D view above it; phones keep the compact height.
-}
drawerHeight : Model -> Int -> Int
drawerHeight model compact =
    if model.width >= 768 then
        max compact (model.height - 120)

    else
        max compact (min (2 * compact) (model.height - 200))


{-| Height of the bottom panel depending on the active tab.
-}
tabDrawerHeight : Model -> BuilderTab -> Int
tabDrawerHeight model tab =
    case tab of
        BuildBricksTab ->
            dockHeight

        AllBricksTab ->
            dockHeight + drawerHeight model catalogDrawerHeight

        _ ->
            drawerHeight model 280


canvasHeight : Model -> Int
canvasHeight model =
    max 160 (model.height - tabDrawerHeight model model.activeTab)


{-| The editor screen: the studio canvas, a top bar (matching Brick Racer) and
the bottom panel.
-}
view : Model -> Html Msg
view model =
    div
        [ Attr.class "brick-editor"
        , Attr.style "position" "fixed"
        , Attr.style "inset" "0"
        , Attr.style "overflow" "hidden"
        , Attr.style "background" "radial-gradient(ellipse 85% 65% at 50% 18%, #1f2e3d 0%, #0d1722 55%, #050b10 100%)"
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
        , bottomPanel model
        ]


{-| Top bar matching Brick Racer's top bar:

  - Left: Check button (`Finish`)
  - Center: Starter hulls (`BasesTab`), Open build (`ImportBuildTab`), Evaluation (`ReviewTab`)
  - Right: Fit view (`FitView`)

-}
topBar : Model -> Html Msg
topBar model =
    let
        lang =
            model.language

        tabBtn tab dataAttr label icon =
            let
                isActive =
                    model.activeTab == tab
            in
            button
                [ Attr.type_ "button"
                , onClick (SelectTab tab)
                , Html.Events.on "pointerdown" (Decode.succeed (SelectTab tab))
                , Attr.attribute dataAttr ""
                , Attr.attribute "aria-label" label
                , Attr.attribute "aria-pressed"
                    (if isActive then
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
                    (if isActive then
                        "2px solid " ++ Brand.legoYellow

                     else
                        "1px solid rgba(255, 255, 255, 0.18)"
                    )
                , Attr.style "background"
                    (if isActive then
                        Brand.legoYellow

                     else
                        "rgba(15, 23, 42, 0.82)"
                    )
                , Attr.style "color"
                    (if isActive then
                        Brand.legoBlack

                     else
                        "#e2e8f0"
                    )
                , Attr.style "cursor" "pointer"
                , Attr.style "pointer-events" "auto"
                , Attr.style "touch-action" "manipulation"
                , Attr.style "transition" "all 0.15s ease"
                ]
                [ icon |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml [] ]
    in
    div
        [ Attr.style "position" "absolute"
        , Attr.style "top" "0"
        , Attr.style "left" "0"
        , Attr.style "right" "0"
        , Attr.style "z-index" "30"
        , Attr.style "display" "flex"
        , Attr.style "flex-wrap" "wrap"
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
                   , Attr.attribute "aria-label" (tr lang "To the shipyard" "Telakalle")
                   , Attr.title (tr lang "To the shipyard" "Telakalle")
                   ]
            )
            [ FeatherIcons.check |> FeatherIcons.withSize 20 |> FeatherIcons.toHtml [] ]
        , div
            [ Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "center"
            , Attr.style "gap" "4px"
            , Attr.style "flex" "1"
            , Attr.style "min-width" "0"
            , Attr.style "overflow-x" "auto"
            ]
            [ tabBtn BasesTab "data-hulls-toggle" (tr lang "Starter hulls" "Aloitusrungot") FeatherIcons.anchor
            , tabBtn ImportBuildTab "data-import-toggle" (tr lang "Open build" "Avaa malli") FeatherIcons.download
            , tabBtn ReviewTab "data-review-toggle" (tr lang "Evaluation" "Arviointi") FeatherIcons.award
            ]
        , div [ Attr.style "margin-left" "auto", Attr.style "display" "flex", Attr.style "align-items" "center", Attr.style "gap" "4px" ]
            [ iconButton (tr lang "Show the whole ship" "Näytä koko laiva") FeatherIcons.maximize (Just FitView)
            ]
        ]


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


{-| Base lock toggle, shown at the far right of the palette row (matching Brick
Racer).
-}
lockButton : Model -> Html Msg
lockButton model =
    let
        lang =
            model.language

        locked =
            model.baseLocked

        label =
            if locked then
                tr lang "Hull locked (click to unlock and edit hull)" "Runko lukittu (klikkaa avataksesi rungon muokkaus)"

            else
                tr lang "Hull unlocked (click to lock hull)" "Runko avattu (klikkaa lukitaksesi runko)"
    in
    button
        [ Attr.type_ "button"
        , onClick ToggleBaseLock
        , Attr.style "display" "inline-flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "width" "40px"
        , Attr.style "height" "40px"
        , Attr.style "flex-shrink" "0"
        , Attr.style "border-radius" "12px"
        , Attr.style "border"
            (if locked then
                "1px solid rgba(255, 255, 255, 0.2)"

             else
                "1px solid #f59e0b"
            )
        , Attr.style "background"
            (if locked then
                "rgba(15, 23, 42, 0.82)"

             else
                "rgba(245, 158, 11, 0.25)"
            )
        , Attr.style "color"
            (if locked then
                "#cbd5e1"

             else
                "#fbbf24"
            )
        , Attr.style "cursor" "pointer"
        , Attr.style "pointer-events" "auto"
        , Attr.title label
        , Attr.attribute "aria-label" label
        ]
        [ (if locked then
            FeatherIcons.lock

           else
            FeatherIcons.unlock
          )
            |> FeatherIcons.withSize 18
            |> FeatherIcons.toHtml []
        ]


{-| Render bottom panel according to active tab (matching Brick Racer).
-}
bottomPanel : Model -> Html Msg
bottomPanel model =
    case model.activeTab of
        BuildBricksTab ->
            dock model

        AllBricksTab ->
            dock model

        BasesTab ->
            viewBasesDrawer model

        ImportBuildTab ->
            viewImportDrawer model

        ReviewTab ->
            viewReviewDrawer model


dock : Model -> Html Msg
dock model =
    let
        isCatalogOpen =
            model.activeTab == AllBricksTab || model.catalogOpen

        height =
            if isCatalogOpen then
                tabDrawerHeight model AllBricksTab

            else
                dockHeight
    in
    div
        [ Attr.style "position" "absolute"
        , Attr.style "left" "0"
        , Attr.style "right" "0"
        , Attr.style "bottom" "0"
        , Attr.style "height" ("calc(" ++ px height ++ " + env(safe-area-inset-bottom, 0px))")
        , Attr.style "box-sizing" "border-box"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "6px"
        , Attr.style "padding" "6px max(8px, env(safe-area-inset-right)) max(6px, env(safe-area-inset-bottom)) max(8px, env(safe-area-inset-left))"
        , Attr.style "background" "rgba(15, 23, 42, 0.94)"
        , Attr.style "border-top" "1px solid rgba(148, 163, 184, 0.25)"
        ]
        ((if isCatalogOpen then
            [ catalogView model ]

          else
            []
         )
            ++ [ modellingControls model, paletteRow model ]
        )


{-| The modelling toolbar above the palette: undo and redo, then the tools
for the selected part. A tool is greyed out while it has nothing to work on.
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
        , Attr.attribute "aria-label" (tr lang "Part modeling tools" "Osan muokkaustyökalut")
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
         , Attr.style "color"
            (if enabled then
                "#f8fafc"

             else
                "#475569"
            )
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
         , Attr.style "transition" "all 0.15s ease"
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
used last, followed by the base lock toggle on the right (matching Brick
Racer).
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
        , div
            [ Attr.style "width" "1px"
            , Attr.style "height" "48px"
            , Attr.style "background" "rgba(255, 255, 255, 0.18)"
            , Attr.style "flex-shrink" "0"
            ]
            []
        , lockButton model
        ]


{-| Opens the full catalog above the palette row, and closes it again.
-}
catalogButton : Model -> Html Msg
catalogButton model =
    let
        isOpen =
            model.activeTab == AllBricksTab || model.catalogOpen
    in
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
            (if isOpen then
                "rgba(250, 200, 10, 0.35)"

             else
                "rgba(250, 200, 10, 0.15)"
            )
        , Attr.style "color" Brand.legoYellow
        , Attr.style "cursor" "pointer"
        , Attr.style "touch-action" "manipulation"
        , Attr.attribute "aria-label" (tr model.language "All bricks" "Kaikki palikat")
        , Attr.attribute "aria-pressed"
            (if isOpen then
                "true"

             else
                "false"
            )
        , Attr.title (tr model.language "Browse all categorized bricks" "Selaa kaikkia osia kategorioittain")
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
                    , tooltip =
                        name
                            ++ " ("
                            ++ number
                            ++ ")"
                            ++ (if isActive then
                                    " [" ++ tr model.language "Active" "Valittu" ++ "]"

                                else
                                    ""
                               )
                    , ariaLabel =
                        (if isActive then
                            "[★] "

                         else
                            ""
                        )
                            ++ tr model.language "Add " "Lisää "
                            ++ name
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
        , Attr.style "height" (px (drawerHeight model catalogDrawerHeight))
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
                    , Attr.attribute "aria-label" (tr lang "Filter bricks" "Suodata palikoita")
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
        , Attr.style "transition" "all 0.15s ease"
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
                    span [ Attr.style "font-size" "20px" ] [ text "🧊" ]
            ]
        , div [ Attr.style "font-size" "11px", Attr.style "font-weight" "700", Attr.style "text-align" "center", Attr.style "line-height" "1.2" ]
            [ text name ]
        , div [ Attr.style "font-size" "9px", Attr.style "color" "#94a3b8" ]
            [ text ("#" ++ number) ]
        ]


{-| Drawer showing the 4 starter hulls (matching Brick Racer's `viewBasesDrawer`).
-}
viewBasesDrawer : Model -> Html Msg
viewBasesDrawer model =
    let
        lang =
            model.language

        renderBaseCard base =
            let
                isCurrent =
                    base.id == model.base.id

                name =
                    baseName lang base
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
                , Attr.style "flex-direction" "column"
                , Attr.style "gap" "8px"
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
                , Attr.style "color" Brand.legoWhite
                , Attr.style "cursor" "pointer"
                , Attr.style "text-align" "left"
                , Attr.style "touch-action" "manipulation"
                ]
                [ div
                    [ Attr.style "width" "100%"
                    , Attr.style "aspect-ratio" "1 / 1"
                    , Attr.style "border-radius" "8px"
                    , Attr.style "overflow" "hidden"
                    , Attr.style "background" "#070c14"
                    , Attr.style "border" "1px solid rgba(255, 255, 255, 0.08)"
                    ]
                    [ Html.img
                        [ Attr.src ("base-previews/" ++ base.id ++ ".png")
                        , Attr.alt name
                        , Attr.style "width" "100%"
                        , Attr.style "height" "100%"
                        , Attr.style "object-fit" "contain"
                        ]
                        []
                    ]
                , div []
                    [ div [ Attr.style "font-weight" "800", Attr.style "font-size" "13px" ] [ text ("⚓ " ++ name) ]
                    , div [ Attr.style "font-size" "11px", Attr.style "color" "#94a3b8", Attr.style "margin-top" "2px" ]
                        [ text (String.fromInt base.partCount ++ tr lang " parts • bow, deck and hull" " osaa • keula, kansi ja runko") ]
                    ]
                ]
    in
    div (drawerStyle (drawerHeight model 280) ++ [ Attr.attribute "data-hulls" "" ])
        [ div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "10px", Attr.style "height" "100%", Attr.style "overflow-y" "auto" ]
            [ h3 [ Attr.style "font-size" "13px", Attr.style "font-weight" "800", Attr.style "color" Brand.legoYellow, Attr.style "text-transform" "uppercase", Attr.style "margin" "0" ]
                [ text (tr lang "Choose Starter Hull" "Valitse aloitusrunko") ]
            , div [ Attr.style "display" "grid", Attr.style "grid-template-columns" "repeat(2, minmax(0, 1fr))", Attr.style "gap" "10px" ]
                (List.map renderBaseCard Editor.bases)
            ]
        ]


{-| Drawer for pasting build links or LDraw `.ldr` / `.mpd` text (matching
Brick Racer's `viewImportDrawer`).
-}
viewImportDrawer : Model -> Html Msg
viewImportDrawer model =
    let
        lang =
            model.language
    in
    div (drawerStyle (drawerHeight model 280))
        [ div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "10px", Attr.style "max-width" "680px", Attr.style "margin" "0 auto" ]
            [ h3 [ Attr.style "font-size" "13px", Attr.style "font-weight" "800", Attr.style "color" Brand.legoYellow, Attr.style "text-transform" "uppercase", Attr.style "margin" "0" ]
                [ text (tr lang "Open build" "Avaa malli") ]
            , div [ Attr.style "display" "flex", Attr.style "gap" "8px" ]
                [ textarea
                    [ Attr.placeholder (tr lang "Paste a build link (#b/...) or LDraw (.ldr / .mpd) text..." "Liitä rakennuslinkki (#b/...) tai LDraw (.ldr / .mpd) -teksti...")
                    , Attr.value model.pasteText
                    , onInput PasteTextChanged
                    , Attr.style "flex" "1"
                    , Attr.style "height" "70px"
                    , Attr.style "padding" "8px"
                    , Attr.style "border-radius" "8px"
                    , Attr.style "background" "rgba(15, 23, 42, 0.8)"
                    , Attr.style "border" "1px solid rgba(148, 163, 184, 0.3)"
                    , Attr.style "color" Brand.legoWhite
                    , Attr.style "font-family" "monospace"
                    , Attr.style "font-size" "12px"
                    , Attr.style "resize" "none"
                    ]
                    []
                , button
                    [ Attr.type_ "button"
                    , onClick SubmitPaste
                    , Attr.style "padding" "0 16px"
                    , Attr.style "border-radius" "8px"
                    , Attr.style "background" Brand.legoYellow
                    , Attr.style "color" Brand.legoBlack
                    , Attr.style "font-weight" "800"
                    , Attr.style "font-size" "13px"
                    , Attr.style "cursor" "pointer"
                    , Attr.style "border" "none"
                    ]
                    [ text (tr lang "Open" "Avaa") ]
                ]
            , case model.pasteError of
                Just err ->
                    div [ Attr.style "color" "#ef4444", Attr.style "font-size" "12px" ] [ text err ]

                Nothing ->
                    text ""
            ]
        ]


type EvalSeverity
    = EvalBlocker
    | EvalWarning
    | EvalBonus


type alias EvalHint =
    { severity : EvalSeverity
    , title : String
    , detail : Maybe String
    , addParts : Maybe (List String)
    }


{-| Drawer showing ship evaluation, seaworthiness rating, telemetry specs,
one-tap missing-part helpers (mast, sail, cannon, ship's wheel), evaluation
hints with quick-fix buttons, and the Set Sail CTA (matching Brick Racer's
`viewReviewDrawer`).
-}
viewReviewDrawer : Model -> Html Msg
viewReviewDrawer model =
    let
        lang =
            model.language

        title =
            baseName lang model.base

        doc =
            Core.getDocument model.studio

        allParts =
            Document.flattenedPlacements doc
                |> List.map (.part >> String.toLower)

        hasShipsWheel =
            List.member "3829c01.dat" allParts

        spec =
            Ingest.ingestDocument title doc

        report =
            Analyze.analyzeShip spec

        hasBlocking =
            spec.hullPartCount < 2

        ready =
            not hasBlocking && spec.mastPartCount >= 1 && spec.sailPartCount >= 1

        starCount =
            if hasBlocking then
                1

            else
                2
                    + (if spec.mastPartCount >= 1 then
                        1

                       else
                        0
                      )
                    + (if spec.sailPartCount >= 1 then
                        1

                       else
                        0
                      )
                    + (if spec.cannonCount >= 1 || hasShipsWheel then
                        1

                       else
                        0
                      )

        statusChip =
            div
                [ Attr.style "padding" "4px 10px"
                , Attr.style "border-radius" "8px"
                , Attr.style "font-size" "11px"
                , Attr.style "font-weight" "800"
                , Attr.style "white-space" "nowrap"
                , Attr.style "background"
                    (if ready then
                        "rgba(34, 197, 94, 0.22)"

                     else if hasBlocking then
                        "rgba(239, 68, 68, 0.22)"

                     else
                        "rgba(245, 158, 11, 0.22)"
                    )
                , Attr.style "color"
                    (if ready then
                        "#22c55e"

                     else if hasBlocking then
                        "#ef4444"

                     else
                        "#f59e0b"
                    )
                ]
                [ text
                    (if ready then
                        tr lang "Ready to sail" "Purjehdusvalmis"

                     else if hasBlocking then
                        tr lang "Needs fix" "Korjausta vailla"

                     else
                        tr lang "Needs sails" "Kaipaa purjeita"
                    )
                ]

        stars =
            div
                [ Attr.attribute "role" "img"
                , Attr.attribute "aria-label" (tr lang "Quality" "Laatu" ++ ": " ++ String.fromInt starCount ++ " / 5")
                , Attr.style "display" "flex"
                , Attr.style "gap" "4px"
                , Attr.style "font-size" "28px"
                , Attr.style "line-height" "1"
                ]
                (List.map
                    (\n ->
                        span
                            [ Attr.style "color"
                                (if n <= starCount then
                                    "#facc15"

                                 else
                                    "#334155"
                                )
                            ]
                            [ text "★" ]
                    )
                    (List.range 1 5)
                )

        fact lbl val =
            div
                [ Attr.style "flex" "1"
                , Attr.style "min-width" "0"
                , Attr.style "padding" "8px 10px"
                , Attr.style "border-radius" "10px"
                , Attr.style "background" "rgba(15, 23, 42, 0.75)"
                , Attr.style "border" "1px solid rgba(255, 255, 255, 0.08)"
                ]
                [ div [ Attr.style "font-size" "11px", Attr.style "color" "#94a3b8" ] [ text lbl ]
                , div [ Attr.style "font-size" "15px", Attr.style "font-weight" "800", Attr.style "color" Brand.legoWhite, Attr.style "overflow-wrap" "anywhere" ] [ text val ]
                ]

        -- Icon button that adds missing parts, drawn with the target part's real
        -- LDraw thumbnail (matching Brick Racer's helperBtn).
        helperBtn label thumbFile partFiles =
            button
                [ Attr.type_ "button"
                , onClick (AddPartList partFiles)
                , Attr.title label
                , Attr.attribute "aria-label" label
                , Attr.style "position" "relative"
                , Attr.style "width" "56px"
                , Attr.style "height" "56px"
                , Attr.style "padding" "4px"
                , Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                , Attr.style "justify-content" "center"
                , Attr.style "border-radius" "10px"
                , Attr.style "background" "rgba(15, 23, 42, 0.75)"
                , Attr.style "border" "1px solid rgba(250, 200, 10, 0.5)"
                , Attr.style "cursor" "pointer"
                ]
                [ case Dict.get (PartCard.thumbnailKey thumbFile model.colour) model.thumbnails of
                    Just url ->
                        Html.img
                            [ Attr.src url
                            , Attr.alt label
                            , Attr.style "max-width" "100%"
                            , Attr.style "max-height" "100%"
                            , Attr.style "object-fit" "contain"
                            ]
                            []

                    Nothing ->
                        span [ Attr.style "font-size" "18px" ] [ text "⛵" ]
                , span
                    [ Attr.style "position" "absolute"
                    , Attr.style "top" "-6px"
                    , Attr.style "right" "-6px"
                    , Attr.style "width" "18px"
                    , Attr.style "height" "18px"
                    , Attr.style "border-radius" "50%"
                    , Attr.style "display" "flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "justify-content" "center"
                    , Attr.style "background" Brand.legoYellow
                    , Attr.style "color" Brand.legoBlack
                    , Attr.style "font-size" "13px"
                    , Attr.style "font-weight" "900"
                    ]
                    [ text "+" ]
                ]

        sailFixFiles =
            if spec.mastPartCount == 0 then
                [ "3957a.dat", "2335.dat" ]

            else
                [ "2335.dat" ]

        missingButtons =
            List.filterMap identity
                [ if spec.mastPartCount == 0 then
                    Just (helperBtn (tr lang "Add mast" "Lisää masto") "3957a.dat" [ "3957a.dat" ])

                  else
                    Nothing
                , if spec.sailPartCount == 0 then
                    Just (helperBtn (tr lang "Add sail" "Lisää purje") "2335.dat" sailFixFiles)

                  else
                    Nothing
                , if spec.cannonCount == 0 then
                    Just (helperBtn (tr lang "Add cannon" "Lisää tykki") "32828.dat" [ "32828.dat" ])

                  else
                    Nothing
                , if not hasShipsWheel then
                    Just (helperBtn (tr lang "Add ship's wheel" "Lisää ruori") "3829c01.dat" [ "3829c01.dat" ])

                  else
                    Nothing
                ]

        missingSection =
            if List.isEmpty missingButtons then
                text ""

            else
                div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "6px" ]
                    [ sectionLabel (tr lang "Missing parts" "Puuttuvat osat")
                    , div [ Attr.style "display" "flex", Attr.style "flex-wrap" "wrap", Attr.style "gap" "8px" ] missingButtons
                    ]

        hints : List EvalHint
        hints =
            List.filterMap identity
                [ if hasBlocking then
                    Just
                        { severity = EvalBlocker
                        , title = tr lang "Insufficient hull volume to float" "Liian vähän runkoa kellumiseen"
                        , detail = Just (tr lang "Add hull bricks or slopes to form a buoyant hull." "Lisää runkopalikoita tai viisteitä kantavaksi rungoksi.")
                        , addParts = Just [ "3001.dat", "3039.dat" ]
                        }

                  else
                    Nothing
                , if spec.mastPartCount == 0 then
                    Just
                        { severity = EvalWarning
                        , title = tr lang "No mast detected" "Mastoa ei löytynyt"
                        , detail = Just (tr lang "Add an antenna mast (3957a) or mast pillar (43888) on the deck." "Lisää antennimasto (3957a) tai mastopylväs (43888) kannelle.")
                        , addParts = Just [ "3957a.dat" ]
                        }

                  else
                    Nothing
                , if spec.sailPartCount == 0 then
                    Just
                        { severity = EvalWarning
                        , title = tr lang "No sail detected — ship will only drift" "Purjetta ei löytynyt — laiva vain ajelehtii"
                        , detail = Just (tr lang "Attach a sail (flag 2335) to the mast to catch the wind." "Kiinnitä purje (lippu 2335) mastoon ottamaan tuulta.")
                        , addParts = Just sailFixFiles
                        }

                  else
                    Nothing
                , if spec.cannonCount == 0 then
                    Just
                        { severity = EvalWarning
                        , title = tr lang "Unarmed vessel — no cannons on deck" "Aseistamaton alus — ei tykkejä kannella"
                        , detail = Just (tr lang "Add a cannon (32828) to shoot sea targets for time bonuses." "Lisää tykki (32828) ampuaksesi maalitauluja aikabonuksiksi.")
                        , addParts = Just [ "32828.dat" ]
                        }

                  else
                    Nothing
                , if not hasShipsWheel then
                    Just
                        { severity = EvalWarning
                        , title = tr lang "No ship's wheel on deck" "Ei ruoria kannella"
                        , detail = Just (tr lang "Add a ship's wheel (3829c01) to complete the helm." "Lisää ruori (3829c01) täydentämään ohjaamo.")
                        , addParts = Just [ "3829c01.dat" ]
                        }

                  else
                    Nothing
                , if spec.mastPartCount > 0 && spec.sailPartCount > 0 then
                    Just
                        { severity = EvalBonus
                        , title =
                            if report.sailAreaDisplacementRatio > 20.0 then
                                tr lang "Corsair rig — swift in light and moderate winds" "Korsaaritakila — nopea kevyessä ja kohtalaisessa tuulessa"

                            else
                                tr lang "Galleon rig — steady and forgiving in heavy seas" "Kaljuunatakila — vakaa kovassa merenkäynnissä"
                        , detail =
                            Just
                                (tr lang
                                    ("Sail area: " ++ String.fromInt (round spec.sailAreaSqMeters) ++ " m² • Hull speed: " ++ String.fromInt (round report.maxHullSpeedKnots) ++ " kn")
                                    ("Purjepinta-ala: " ++ String.fromInt (round spec.sailAreaSqMeters) ++ " m² • Runkonopeus: " ++ String.fromInt (round report.maxHullSpeedKnots) ++ " solmua")
                                )
                        , addParts = Nothing
                        }

                  else
                    Nothing
                , if spec.cannonCount > 0 then
                    Just
                        { severity = EvalBonus
                        , title =
                            tr lang
                                ("Broadside battery ready (" ++ String.fromInt spec.cannonCount ++ " cannons)")
                                ("Täyslaidallinen valmiina (" ++ String.fromInt spec.cannonCount ++ " tykkiä)")
                        , detail = Just (tr lang "Tap water or press Space while sailing to fire." "Napauta vettä tai paina välilyöntiä purjehtiessa ampuaksesi.")
                        , addParts = Nothing
                        }

                  else
                    Nothing
                ]

        severityColour sev =
            case sev of
                EvalBlocker ->
                    "#ef4444"

                EvalWarning ->
                    "#f59e0b"

                EvalBonus ->
                    "#22c55e"

        fixButton files =
            button
                [ Attr.type_ "button"
                , onClick (AddPartList files)
                , Attr.style "flex-shrink" "0"
                , Attr.style "padding" "4px 10px"
                , Attr.style "border-radius" "8px"
                , Attr.style "background" "rgba(15, 23, 42, 0.75)"
                , Attr.style "border" "1px solid rgba(250, 200, 10, 0.5)"
                , Attr.style "color" Brand.legoYellow
                , Attr.style "font-size" "12px"
                , Attr.style "font-weight" "800"
                , Attr.style "cursor" "pointer"
                ]
                [ text (tr lang "Add" "Lisää") ]

        issuesSection =
            if List.isEmpty hints then
                div [ Attr.style "font-size" "13px", Attr.style "color" "#22c55e" ]
                    [ text (tr lang "No issues found" "Ei huomautuksia") ]

            else
                div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "6px" ]
                    (List.map
                        (\hint ->
                            div
                                [ Attr.style "display" "flex"
                                , Attr.style "align-items" "center"
                                , Attr.style "gap" "8px"
                                , Attr.style "font-size" "13px"
                                , Attr.style "color" "#e2e8f0"
                                ]
                                [ span
                                    [ Attr.style "width" "8px"
                                    , Attr.style "height" "8px"
                                    , Attr.style "border-radius" "50%"
                                    , Attr.style "flex-shrink" "0"
                                    , Attr.style "background" (severityColour hint.severity)
                                    ]
                                    []
                                , div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "2px", Attr.style "flex" "1", Attr.style "min-width" "0" ]
                                    [ text hint.title
                                    , case hint.detail of
                                        Just detail ->
                                            span [ Attr.style "font-size" "11px", Attr.style "color" "#94a3b8" ] [ text detail ]

                                        Nothing ->
                                            text ""
                                    ]
                                , case hint.addParts of
                                    Just files ->
                                        fixButton files

                                    Nothing ->
                                        text ""
                                ]
                        )
                        hints
                    )
    in
    div (drawerStyle (drawerHeight model 280))
        [ div [ Attr.style "display" "flex", Attr.style "flex-direction" "column", Attr.style "gap" "12px", Attr.style "height" "100%", Attr.style "overflow-y" "auto" ]
            [ div [ Attr.style "display" "flex", Attr.style "align-items" "center", Attr.style "justify-content" "space-between", Attr.style "gap" "8px" ]
                [ h3 [ Attr.style "font-size" "14px", Attr.style "font-weight" "800", Attr.style "color" Brand.legoYellow, Attr.style "margin" "0" ]
                    [ text title ]
                , statusChip
                ]
            , stars
            , div [ Attr.style "display" "flex", Attr.style "gap" "8px" ]
                [ fact (tr lang "Sails" "Purjeet") (String.fromInt spec.sailPartCount ++ tr lang " pcs" " kpl")
                , fact (tr lang "Cannons" "Tykit") (String.fromInt spec.cannonCount ++ tr lang " pcs" " kpl")
                , fact (tr lang "Displacement" "Uppouma") (String.fromInt (round spec.mass) ++ " kg")
                ]
            , missingSection
            , issuesSection
            , if not hasBlocking then
                button
                    [ Attr.type_ "button"
                    , onClick Finish
                    , Attr.style "width" "100%"
                    , Attr.style "padding" "10px 16px"
                    , Attr.style "border-radius" "12px"
                    , Attr.style "background" Brand.legoYellow
                    , Attr.style "color" Brand.legoBlack
                    , Attr.style "font-size" "14px"
                    , Attr.style "font-weight" "800"
                    , Attr.style "cursor" "pointer"
                    , Attr.style "border" "none"
                    , Attr.style "display" "flex"
                    , Attr.style "align-items" "center"
                    , Attr.style "justify-content" "center"
                    , Attr.style "gap" "8px"
                    , Attr.style "box-shadow" "0 4px 14px rgba(0, 0, 0, 0.3)"
                    ]
                    [ FeatherIcons.play |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml []
                    , span [] [ text (tr lang "Use this ship" "Käytä tätä laivaa") ]
                    ]

              else
                text ""
            ]
        ]


sectionLabel : String -> Html msg
sectionLabel label =
    div [ Attr.style "font-size" "11px", Attr.style "font-weight" "800", Attr.style "color" "#94a3b8", Attr.style "text-transform" "uppercase", Attr.style "letter-spacing" "0.5px" ]
        [ text label ]


drawerStyle : Int -> List (Html.Attribute Msg)
drawerStyle heightPx =
    [ Attr.style "position" "absolute"
    , Attr.style "left" "0"
    , Attr.style "right" "0"
    , Attr.style "bottom" "0"
    , Attr.style "height" (px heightPx)
    , Attr.style "box-sizing" "border-box"
    , Attr.style "padding" "12px max(16px, env(safe-area-inset-right)) max(12px, env(safe-area-inset-bottom)) max(16px, env(safe-area-inset-left))"
    , Attr.style "background" "rgba(5, 19, 29, 0.96)"
    , Attr.style "border-top" "1px solid rgba(255, 255, 255, 0.15)"
    , Attr.style "box-shadow" "0 -4px 16px rgba(0, 0, 0, 0.4)"
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
