module Sails.I18n exposing
    ( allPhrases
    , backToBoats
    , backToCourse
    , bestLine
    , boatDescription
    , boatName
    , buildOwnShip
    , controlsHint
    , copyLink
    , copyRoomLink
    , countdownHint
    , courseTitle
    , crewHeading
    , editShipButton
    , exitRace
    , finalTimeLabel
    , finishToast
    , finishedBadge
    , fireButton
    , fireTitle
    , fromFlag
    , goBanner
    , gridHost
    , gridHostDesc
    , harbourAria
    , harbourCount
    , harbourTimeTitle
    , harbourToast
    , hazardPounding
    , hazardRocks
    , headline
    , heelLabel
    , heelTitle
    , hitToast
    , hitsStat
    , hostTitle
    , hullDamagedToast
    , hullLabel
    , hullTitle
    , joinRace
    , joinRaceDesc
    , joinRaceTitle
    , loadingBricks
    , loadingShips
    , mainMenuButton
    , mooringHint
    , muteLabel
    , newPersonalBest
    , nextGoal
    , nextHarbourTitle
    , overpoweredTitle
    , personalBestLine
    , personalBestShort
    , pirateSunkToast
    , piratesBoardedToast
    , playerCount
    , pointOfSailText
    , raceStatus
    , reefWarning
    , restartRace
    , roomCodeLabel
    , roomCodePlaceholder
    , roomLabel
    , sailAgain
    , sailLevelName
    , sailLevelTitle
    , sailStep
    , sailingStat
    , setSail
    , shipLoadError
    , shipwrecked
    , shipwreckedText
    , slotLabel
    , slotOpen
    , spaceKey
    , speedKnotsTitle
    , speedLabel
    , starsAria
    , startRace
    , steerLabel
    , subtitle
    , targetsLabel
    , title
    , toCode
    , treasureLabel
    , treasureStat
    , treasureToast
    , turnLabel
    , twoPlayerHost
    , twoPlayerHostDesc
    , weatherName
    , windDirection
    , you
    )

{-| Finnish and English copy of every piece of the Brick Sails interface.

Finnish is the default, like in the other Brick Frame games: the language
comes from the `language` flag the shared boot code reads from a `?lang=`
link or a saved choice (`runtime/language.js`), and only a code starting with
"en" picks English. The `Language` type is the shared one of `brick-frame-ui`.

Use the shared `I18n.Language` type (`Fi` / `En`) with it.

Boat names, the course and the weather keep their English names inside the
game (they are keys of saved data and of the multiplayer messages); only what
the player reads is translated here.

-}

import I18n exposing (Language(..))



-- LANGUAGE


{-| Language of a `language` flag: English for a code starting with "en",
Finnish for anything else (Finnish is the default).
-}
fromFlag : String -> Language
fromFlag code =
    if String.startsWith "en" (String.toLower (String.trim code)) then
        En

    else
        Fi


{-| Code saved for a language ("fi" or "en").
-}
toCode : Language -> String
toCode lang =
    case lang of
        Fi ->
            "fi"

        En ->
            "en"


pick : Language -> String -> String -> String
pick lang fi en =
    case lang of
        Fi ->
            fi

        En ->
            en



-- SHIPYARD


title : Language -> String
title lang =
    pick lang "Palikkapurjehdus" "Brick Sails"


subtitle : Language -> String
subtitle lang =
    pick lang "Leluveneiden meriseikkailu" "Toy boat sea adventure"


courseTitle : Language -> String -> String
courseTitle lang name =
    case ( lang, name ) of
        ( Fi, "Island Loop" ) ->
            "Aarresaarten kierros"

        ( En, "Island Loop" ) ->
            "Treasure Isles Loop"

        _ ->
            name


{-| Shown name of a boat. Built-in boats keep their English `name` as their
key; a loaded build is "Your ship".
-}
boatName : Language -> String -> String
boatName lang name =
    case lang of
        En ->
            name

        Fi ->
            case name of
                "Sloop" ->
                    "Slooppi"

                "Cutter" ->
                    "Kutteri"

                "Schooner" ->
                    "Kuunari"

                "Catamaran" ->
                    -- A soft hyphen lets the narrow boat button break it.
                    "Kata\u{00AD}maraani"

                "Your ship" ->
                    "Oma laiva"

                _ ->
                    name


{-| Shown description of a boat (see `Sails.Boats`).
-}
boatDescription : Language -> String -> String
boatDescription lang description =
    case lang of
        En ->
            description

        Fi ->
            case description of
                "Balanced all-rounder." ->
                    "Tasapainoinen monitoimija."

                "Nimble: turns on a stud." ->
                    "Ketterä: kääntyy nupin päällä."

                "Two masts, quick on a reach." ->
                    "Kaksi mastoa, nopea sivutuulessa."

                "Fastest, but turns wide." ->
                    "Nopein, mutta kääntyy laajalla kaarella."

                "Your own build." ->
                    "Oma rakennelmasi."

                _ ->
                    if String.startsWith "Your build: " description then
                        "Rakennelmasi: " ++ String.dropLeft (String.length "Your build: ") description

                    else
                        description


loadingBricks : Language -> Int -> Int -> String
loadingBricks lang loaded total =
    pick lang "Ladataan palikoita " "Loading bricks "
        ++ String.fromInt loaded
        ++ " / "
        ++ String.fromInt total


muteLabel : Language -> Bool -> String
muteLabel lang muted =
    if muted then
        pick lang "Äänet päälle" "Unmute"

    else
        pick lang "Mykistä" "Mute"


stars : Int -> String
stars n =
    String.repeat n "★" ++ String.repeat (3 - n) "☆"


{-| Best time line of the Shipyard, with its stars.
-}
personalBestLine : Language -> String -> Int -> String
personalBestLine lang time starCount =
    pick lang "Paras " "Best " ++ time ++ " · " ++ stars starCount


{-| Goal line of the Shipyard while the course has no best time yet.
-}
bestLine : Language -> String -> String
bestLine lang target =
    pick lang "3 tähteä alle " "3 stars under " ++ target


controlsHint : Language -> Bool -> String
controlsHint lang tilt =
    if tilt then
        pick lang
            "Kallista puhelinta ohjataksesi. ⇑ ⇓ nostaa ja laskee purjetta. Napauta AMMU, kun olet lähellä kelluvia maalitauluja, kapteeni."
            "Tilt the phone to steer. ⇑ ⇓ raise and lower the sail. Tap FIRE when you're near the floating targets, captain."

    else
        pick lang
            "Pidä ◀ ▶ (tai nuolinäppäimiä) pohjassa ohjataksesi. Nosta / laske purjetta ⇑ ⇓ (W / S). AMMU (välilyönti) kelluvien maalitaulujen lähellä."
            "Hold ◀ ▶ (or arrow keys) to steer. Raise / lower sail with ⇑ ⇓ (W / S). FIRE (Space) to blast nearby floating targets."


speedLabel : Language -> String
speedLabel lang =
    pick lang "Nopeus" "Speed"


turnLabel : Language -> String
turnLabel lang =
    pick lang "Käännös" "Turn"


setSail : Language -> String
setSail lang =
    pick lang "Purjehdi" "Set sail"


twoPlayerHost : Language -> String
twoPlayerHost lang =
    pick lang "Kaksinpeli" "Host 2-player"


gridHost : Language -> String
gridHost lang =
    pick lang "Moninpeli (8)" "Host 8-player"


{-| Tooltip of a host button.
-}
hostTitle : Language -> Int -> String
hostTitle lang players =
    pick lang
        ("Avaa " ++ String.fromInt players ++ " pelaajan huone tälle näytölle")
        ("Host a " ++ String.fromInt players ++ "-player room on this screen")


roomCodePlaceholder : Language -> String
roomCodePlaceholder lang =
    pick lang "Huoneen koodi tai linkki" "Room code or link"


roomCodeLabel : Language -> String
roomCodeLabel lang =
    pick lang "Huoneen koodi" "Room code"


joinRaceTitle : Language -> String
joinRaceTitle lang =
    pick lang "Liity avattuun seikkailuun" "Join a hosted voyage"


joinRace : Language -> String
joinRace lang =
    pick lang "Liity miehistöön" "Join the crew"


{-| A loaded build that could not be used, in the player's language. The
reasons come from `Sails.Ingest` in English.
-}
shipLoadError : Language -> String -> String
shipLoadError lang err =
    case lang of
        En ->
            err

        Fi ->
            case err of
                "Paste a build link first." ->
                    "Liitä ensin rakennelinkki."

                "That build link is damaged and could not be read." ->
                    "Rakennelinkki on vioittunut, eikä sitä voitu lukea."

                "That is not a build link. Copy the link from the building app." ->
                    "Tämä ei ole rakennelinkki. Kopioi linkki rakennussovelluksesta."

                "That build has no bricks." ->
                    "Rakennelmassa ei ole palikoita."

                _ ->
                    if String.startsWith "That build is too big" err then
                        case List.filterMap String.toInt (String.words (String.map digitsOnly err)) of
                            [ count, most ] ->
                                "Rakennelma on liian suuri purjehdittavaksi ("
                                    ++ String.fromInt count
                                    ++ " osaa, enintään "
                                    ++ String.fromInt most
                                    ++ ")."

                            _ ->
                                "Rakennelma on liian suuri purjehdittavaksi."

                    else if String.startsWith "Could not open that file: " err then
                        "Tiedostoa ei voitu avata: " ++ String.dropLeft (String.length "Could not open that file: ") err

                    else if String.startsWith "Not a boat: " err then
                        "Ei vene: " ++ unseaworthyReason (String.dropLeft (String.length "Not a boat: ") err)

                    else
                        err


digitsOnly : Char -> Char
digitsOnly c =
    if Char.isDigit c then
        c

    else
        ' '


unseaworthyReason : String -> String
unseaworthyReason reason =
    case reason of
        "Ship has insufficient hull volume to float upright." ->
            "rungossa ei ole tarpeeksi tilavuutta, jotta laiva kelluisi pystyssä."

        _ ->
            reason



-- HUD


pointOfSailText : Language -> { inIrons : Bool, closeHauled : Bool, reaching : Bool } -> String
pointOfSailText lang p =
    if p.inIrons then
        pick lang "Vastatuuli: käänny pois" "Into wind: turn away"

    else if p.closeHauled then
        pick lang "Kryssi" "Close-hauled"

    else if p.reaching then
        pick lang "Sivutuuli: nopein" "Reach: fastest"

    else
        pick lang "Myötätuuli" "Running"


harbourAria : Language -> String
harbourAria lang =
    pick lang "Satamat" "Harbours"


harbourCount : Language -> Int -> Int -> String
harbourCount lang n total =
    pick lang "Satama " "Harbour " ++ String.fromInt n ++ "/" ++ String.fromInt total


windDirection : Language -> String
windDirection lang =
    pick lang "Tuulen suunta" "Wind direction"


treasureLabel : Language -> Int -> Int -> String
treasureLabel lang count bonus =
    pick lang "Aarteet " "Treasure " ++ String.fromInt count ++ " (-" ++ String.fromInt bonus ++ "s)"


{-| Targets hit so far, with the seconds they take off.
-}
targetsLabel : Language -> Int -> Int -> String
targetsLabel lang count bonus =
    pick lang "Osumat " "Targets " ++ String.fromInt count ++ " (-" ++ String.fromInt bonus ++ "s)"


{-| Shown name of the weather (`Sails.Weather.label` gives the English one).
-}
weatherName : Language -> String -> String
weatherName lang name =
    case lang of
        En ->
            name

        Fi ->
            case name of
                "Squall" ->
                    "Puuska"

                "Thunderstorm" ->
                    "Ukkosmyrsky"

                "Calm before the storm" ->
                    "Tyyntä ennen myrskyä"

                "Clearing" ->
                    "Selkenee"

                _ ->
                    name


restartRace : Language -> String
restartRace lang =
    pick lang "Aloita seikkailu alusta" "Restart voyage"


backToBoats : Language -> String
backToBoats lang =
    pick lang "Takaisin veneisiin" "Back to boats"


{-| The button that leaves the race, shown once the boat has stopped.
-}
exitRace : Language -> String
exitRace lang =
    pick lang "Poistu" "Exit"


steerLabel : Language -> Bool -> String
steerLabel lang left =
    if left then
        pick lang "Ohjaa vasemmalle" "Steer left"

    else
        pick lang "Ohjaa oikealle" "Steer right"


fireTitle : Language -> String
fireTitle lang =
    pick lang "Ammu tykeillä" "Fire cannons"


{-| The space bar, as named in a keyboard hint.
-}
spaceKey : Language -> String
spaceKey lang =
    pick lang "välilyönti" "Space"


{-| Text of the FIRE button: fire when loaded, reloading otherwise.
-}
fireButton : Language -> Bool -> String
fireButton lang ready =
    if ready then
        pick lang "AMMU" "FIRE"

    else
        pick lang "LATAA" "LOAD"


goBanner : Language -> String
goBanner lang =
    pick lang "ANKKURIT YLÖS!" "ANCHORS AWEIGH!"


countdownHint : Language -> String
countdownHint lang =
    pick lang
        "Poikkea pasaatituulessa jokaiseen satamaan vuorollaan: hidasta hehkuvan renkaan sisällä ja kiinnity. Mene lähelle maalitaulua ja AMMU: −2 s."
        "Ride the trade winds to each harbour in turn: slow down inside the glowing ring to moor. Get close to a target and FIRE for -2 s."


{-| The sail buttons: raise (True) or lower (False).
-}
sailStep : Language -> Bool -> String
sailStep lang raise =
    if raise then
        pick lang "Nosta purjetta" "Raise sail"

    else
        pick lang "Laske purjetta" "Lower sail"


{-| Shown name of a sail setting (`Sails.Physics.sailLevels` names them in
English).
-}
sailLevelName : Language -> String -> String
sailLevelName lang name =
    case lang of
        En ->
            name

        Fi ->
            case name of
                "Down" ->
                    "Alas"

                "Storm" ->
                    "Myrsky"

                "Reefed" ->
                    "Reivi"

                "Full" ->
                    "Täysi"

                "Press" ->
                    "Kiri"

                _ ->
                    name


sailLevelTitle : Language -> String -> String
sailLevelTitle lang name =
    pick lang "Purje: " "Sail: " ++ sailLevelName lang name


overpoweredTitle : Language -> String
overpoweredTitle lang =
    pick lang "Liikaa purjetta: laske purjetta" "Overpowered: lower the sail"


reefWarning : Language -> String
reefWarning lang =
    pick lang "Reivaa!" "Reef!"


hullLabel : Language -> String
hullLabel lang =
    pick lang "Runko" "Hull"


hullTitle : Language -> String
hullTitle lang =
    pick lang
        "Runko: kivet, tykinkuulat ja myrskyaallot kuluttavat sitä; se korjaantuu, kun sitä ei rasiteta"
        "Hull: rocks, cannonballs and storm waves wear it down; it mends when left alone"


{-| How far the boat heels, with a warning to ease the sail when it is hard
over.
-}
heelLabel : Language -> Int -> Bool -> String
heelLabel lang degrees hard =
    pick lang "Kallistuma " "Heel "
        ++ String.fromInt degrees
        ++ "°"
        ++ (if hard then
                pick lang " · löysää purjetta" " · ease the sail"

            else
                ""
           )


{-| Single-word hazard warnings under the hull bar: the boat has struck
rock, or a storm sea is pounding the hull.
-}
hazardRocks : Language -> String
hazardRocks lang =
    pick lang "KARI" "ROCKS"


hazardPounding : Language -> String
hazardPounding lang =
    pick lang "AALLOKKO" "POUNDING"


speedKnotsTitle : Language -> String
speedKnotsTitle lang =
    pick lang "Nopeus solmuina" "Speed in knots"


nextHarbourTitle : Language -> String
nextHarbourTitle lang =
    pick lang "Matka seuraavaan satamaan" "Distance to the next harbour"


{-| Tooltip of the harbour time that shows for a few seconds after docking.
-}
harbourTimeTitle : Language -> Int -> String
harbourTimeTitle lang n =
    pick lang ("Aika satamassa " ++ String.fromInt n) ("Time at harbour " ++ String.fromInt n)


{-| Shown in place of the distance while the boat is mooring at a harbour.
-}
mooringHint : Language -> String
mooringHint lang =
    pick lang "Kiinnitetään…" "Mooring…"


heelTitle : Language -> String
heelTitle lang =
    pick lang "Kallistuma: laske purjetta, jos vene kallistuu rajusti" "Heel: lower the sail if the boat leans over hard"


backToCourse : Language -> String
backToCourse lang =
    pick lang "Takaisin reitille" "Back on course"


shipwrecked : Language -> String
shipwrecked lang =
    pick lang "Haaksirikko!" "Shipwrecked!"


shipwreckedText : Language -> String
shipwreckedText lang =
    pick lang
        "Laivasi hajosi palikoiksi riutalla. Tämä seikkailu päättyi: väistä riuttoja ja ota isot aallot rauhallisesti."
        "Your ship broke apart on the reef. This voyage is lost: steer clear of the reefs and ease into big waves."


sailAgain : Language -> String
sailAgain lang =
    pick lang "Uudestaan" "Sail again"


{-| The finish screen's button back to the main menu (the shipyard).
-}
mainMenuButton : Language -> String
mainMenuButton lang =
    pick lang "Valikkoon" "Main menu"


{-| Label of the final time on the finish screen.
-}
finalTimeLabel : Language -> String
finalTimeLabel lang =
    pick lang "Loppuaika" "Final time"


{-| Finish screen stat: time spent sailing.
-}
sailingStat : Language -> String
sailingStat lang =
    pick lang "Purjehdus" "Sailing"


{-| Finish screen stat: targets hit.
-}
hitsStat : Language -> String
hitsStat lang =
    pick lang "Osumat" "Hits"


{-| Finish screen stat: treasure found.
-}
treasureStat : Language -> String
treasureStat lang =
    pick lang "Aarteet" "Treasure"



-- TOASTS


treasureToast : Language -> Int -> String
treasureToast lang bonus =
    pick lang "Aarre! −" "Treasure! −" ++ String.fromInt bonus ++ " s"


finishToast : Language -> String
finishToast lang =
    pick lang "SATAMASSA!" "LAND HO!"


{-| A pirate ship came alongside and boarded the boat.
-}
piratesBoardedToast : Language -> String
piratesBoardedToast lang =
    pick lang "Merirosvot kimppuun!" "Pirates boarded!"


{-| A cannon ball sank a pirate ship, with the seconds it takes off.
-}
pirateSunkToast : Language -> Int -> String
pirateSunkToast lang bonus =
    pick lang "Merirosvo upposi! −" "Pirate sunk! −" ++ String.fromInt bonus ++ " s"


harbourToast : Language -> Int -> Int -> String
harbourToast lang n total =
    pick lang "Satama " "Harbour " ++ String.fromInt n ++ "/" ++ String.fromInt total


hitToast : Language -> Int -> String
hitToast lang bonus =
    pick lang "Osuma! −" "Hit! −" ++ String.fromInt bonus ++ " s"


hullDamagedToast : Language -> Int -> String
hullDamagedToast lang pct =
    pick lang "Runko vaurioitui: " "Hull damaged: " ++ String.fromInt pct ++ "%"



-- RESULTS


{-| Headline of the finish screen by its stars.
-}
headline : Language -> Int -> String
headline lang starCount =
    case starCount of
        3 ->
            pick lang "Merirosvokuninkaan arvoista!" "Fit for a pirate king!"

        2 ->
            pick lang "Hienoa merimiestaitoa!" "Fine seamanship!"

        _ ->
            pick lang "Satamassa!" "Safe in harbour!"


starsAria : Language -> Int -> String
starsAria lang n =
    String.fromInt n ++ pick lang " tähteä" " stars"


nextGoal : Language -> String -> String
nextGoal lang target =
    pick lang "3 tähteä ajalla " "3 stars at " ++ target


newPersonalBest : Language -> String
newPersonalBest lang =
    pick lang "Uusi oma ennätys!" "New personal best!"


{-| The best time so far, on the finish screen.
-}
personalBestShort : Language -> String -> String
personalBestShort lang time =
    pick lang "Paras: " "Best: " ++ time



-- MULTIPLAYER


{-| Ships of other players that are still on their way, by player slot.
-}
loadingShips : Language -> List Int -> String
loadingShips lang slots =
    pick lang "Ladataan laivaa: " "Loading ship: "
        ++ String.join ", " (List.map (\s -> "P" ++ String.fromInt s) slots)
        ++ "…"


copyRoomLink : Language -> String
copyRoomLink lang =
    pick lang "Kopioi huoneen linkki" "Copy room link"


copyLink : Language -> String
copyLink lang =
    pick lang "Kopioi linkki" "Copy link"


startRace : Language -> String
startRace lang =
    pick lang "LÄHDE SEIKKAILUUN" "SET SAIL"


roomLabel : Language -> String -> String
roomLabel lang code =
    pick lang "HUONE: " "ROOM: " ++ code


playerCount : Language -> Int -> Int -> String
playerCount lang n most =
    String.fromInt n ++ "/" ++ String.fromInt most ++ pick lang " pelaajaa" " players"


{-| Status of a running room on the host's header: counting down (`Just n`),
racing or finished.
-}
raceStatus : Language -> { countdown : Maybe Int, finished : Bool } -> String
raceStatus lang s =
    case s.countdown of
        Just n ->
            pick lang "LÄHTÖ " "SAILING IN " ++ String.fromInt n ++ "..."

        Nothing ->
            if s.finished then
                pick lang "SEIKKAILU PÄÄTTYI" "VOYAGE OVER"

            else
                pick lang "SEIKKAILU KÄYNNISSÄ" "VOYAGE UNDER WAY"


slotLabel : Language -> Int -> String
slotLabel lang slot =
    pick lang "Paikka " "Slot " ++ String.fromInt slot


slotOpen : Language -> Int -> String
slotOpen lang slot =
    slotLabel lang slot ++ pick lang " vapaa" " open"


{-| A player's own name tag, "(you)".
-}
you : Language -> String
you lang =
    pick lang "(sinä)" "(you)"


finishedBadge : Language -> String
finishedBadge lang =
    pick lang "SATAMASSA" "IN PORT"



-- CHECKS


{-| Every phrase of the interface in both languages, by key, so a test can
check that none is missing (see `tests/LanguageTest.elm`).
-}
allPhrases : List ( String, Language -> String )
allPhrases =
    [ ( "title", title )
    , ( "subtitle", subtitle )
    , ( "courseTitle", \l -> courseTitle l "Island Loop" )
    , ( "boatName Sloop", \l -> boatName l "Sloop" )
    , ( "boatName Cutter", \l -> boatName l "Cutter" )
    , ( "boatName Schooner", \l -> boatName l "Schooner" )
    , ( "boatName Catamaran", \l -> boatName l "Catamaran" )
    , ( "boatName Your ship", \l -> boatName l "Your ship" )
    , ( "boatDescription Sloop", \l -> boatDescription l "Balanced all-rounder." )
    , ( "boatDescription Cutter", \l -> boatDescription l "Nimble: turns on a stud." )
    , ( "boatDescription Schooner", \l -> boatDescription l "Two masts, quick on a reach." )
    , ( "boatDescription Catamaran", \l -> boatDescription l "Fastest, but turns wide." )
    , ( "boatDescription own", \l -> boatDescription l "Your own build." )
    , ( "boatDescription build", \l -> boatDescription l "Your build: Pole" )
    , ( "loadingBricks", \l -> loadingBricks l 1 2 )
    , ( "muteLabel muted", \l -> muteLabel l True )
    , ( "muteLabel unmuted", \l -> muteLabel l False )
    , ( "personalBestLine", \l -> personalBestLine l "1:00.00" 2 )
    , ( "bestLine", \l -> bestLine l "1:00.00" )
    , ( "controlsHint tilt", \l -> controlsHint l True )
    , ( "controlsHint buttons", \l -> controlsHint l False )
    , ( "speedLabel", speedLabel )
    , ( "turnLabel", turnLabel )
    , ( "setSail", setSail )
    , ( "twoPlayerHost", twoPlayerHost )
    , ( "gridHost", gridHost )
    , ( "hostTitle", \l -> hostTitle l 2 )
    , ( "roomCodePlaceholder", roomCodePlaceholder )
    , ( "roomCodeLabel", roomCodeLabel )
    , ( "joinRaceTitle", joinRaceTitle )
    , ( "joinRace", joinRace )
    , ( "shipLoadError empty", \l -> shipLoadError l "Paste a build link first." )
    , ( "shipLoadError damaged", \l -> shipLoadError l "That build link is damaged and could not be read." )
    , ( "shipLoadError not a link", \l -> shipLoadError l "That is not a build link. Copy the link from the building app." )
    , ( "shipLoadError no bricks", \l -> shipLoadError l "That build has no bricks." )
    , ( "shipLoadError too big", \l -> shipLoadError l "That build is too big to sail here (900 parts, at most 400)." )
    , ( "shipLoadError not a boat", \l -> shipLoadError l "Not a boat: Ship has insufficient hull volume to float upright." )
    , ( "shipLoadError file", \l -> shipLoadError l "Could not open that file: x" )
    , ( "pointOfSail inIrons", \l -> pointOfSailText l { inIrons = True, closeHauled = False, reaching = False } )
    , ( "pointOfSail closeHauled", \l -> pointOfSailText l { inIrons = False, closeHauled = True, reaching = False } )
    , ( "pointOfSail reaching", \l -> pointOfSailText l { inIrons = False, closeHauled = False, reaching = True } )
    , ( "pointOfSail running", \l -> pointOfSailText l { inIrons = False, closeHauled = False, reaching = False } )
    , ( "harbourAria", harbourAria )
    , ( "harbourCount", \l -> harbourCount l 1 7 )
    , ( "windDirection", windDirection )
    , ( "treasureLabel", \l -> treasureLabel l 1 5 )
    , ( "targetsLabel", \l -> targetsLabel l 1 2 )
    , ( "weather Squall", \l -> weatherName l "Squall" )
    , ( "weather Thunderstorm", \l -> weatherName l "Thunderstorm" )
    , ( "weather Calm", \l -> weatherName l "Calm before the storm" )
    , ( "weather Clearing", \l -> weatherName l "Clearing" )
    , ( "restartRace", restartRace )
    , ( "backToBoats", backToBoats )
    , ( "exitRace", exitRace )
    , ( "steerLabel left", \l -> steerLabel l True )
    , ( "steerLabel right", \l -> steerLabel l False )
    , ( "fireTitle", fireTitle )
    , ( "spaceKey", spaceKey )
    , ( "fireButton ready", \l -> fireButton l True )
    , ( "fireButton loading", \l -> fireButton l False )
    , ( "goBanner", goBanner )
    , ( "countdownHint", countdownHint )
    , ( "sailStep raise", \l -> sailStep l True )
    , ( "sailStep lower", \l -> sailStep l False )
    , ( "sailLevel Down", \l -> sailLevelName l "Down" )
    , ( "sailLevel Storm", \l -> sailLevelName l "Storm" )
    , ( "sailLevel Reefed", \l -> sailLevelName l "Reefed" )
    , ( "sailLevel Full", \l -> sailLevelName l "Full" )
    , ( "sailLevel Press", \l -> sailLevelName l "Press" )
    , ( "sailLevelTitle", \l -> sailLevelTitle l "Full" )
    , ( "overpoweredTitle", overpoweredTitle )
    , ( "reefWarning", reefWarning )
    , ( "hullLabel", hullLabel )
    , ( "hullTitle", hullTitle )
    , ( "heelLabel", \l -> heelLabel l 25 True )
    , ( "heelTitle", heelTitle )
    , ( "hazardRocks", hazardRocks )
    , ( "hazardPounding", hazardPounding )
    , ( "speedKnotsTitle", speedKnotsTitle )
    , ( "nextHarbourTitle", nextHarbourTitle )
    , ( "harbourTimeTitle", \l -> harbourTimeTitle l 2 )
    , ( "mooringHint", mooringHint )
    , ( "backToCourse", backToCourse )
    , ( "shipwrecked", shipwrecked )
    , ( "shipwreckedText", shipwreckedText )
    , ( "sailAgain", sailAgain )
    , ( "mainMenuButton", mainMenuButton )
    , ( "piratesBoardedToast", piratesBoardedToast )
    , ( "pirateSunkToast", \l -> pirateSunkToast l 2 )
    , ( "finalTimeLabel", finalTimeLabel )
    , ( "sailingStat", sailingStat )
    , ( "hitsStat", hitsStat )
    , ( "treasureStat", treasureStat )
    , ( "treasureToast", \l -> treasureToast l 5 )
    , ( "finishToast", finishToast )
    , ( "harbourToast", \l -> harbourToast l 1 7 )
    , ( "hitToast", \l -> hitToast l 2 )
    , ( "hullDamagedToast", \l -> hullDamagedToast l 80 )
    , ( "headline 3", \l -> headline l 3 )
    , ( "headline 2", \l -> headline l 2 )
    , ( "headline 1", \l -> headline l 1 )
    , ( "starsAria", \l -> starsAria l 2 )
    , ( "nextGoal", \l -> nextGoal l "1:00.00" )
    , ( "newPersonalBest", newPersonalBest )
    , ( "personalBestShort", \l -> personalBestShort l "1:00.00" )
    , ( "loadingShips", \l -> loadingShips l [ 2 ] )
    , ( "copyRoomLink", copyRoomLink )
    , ( "copyLink", copyLink )
    , ( "startRace", startRace )
    , ( "roomLabel", \l -> roomLabel l "ABCD" )
    , ( "playerCount", \l -> playerCount l 2 8 )
    , ( "raceStatus countdown", \l -> raceStatus l { countdown = Just 3, finished = False } )
    , ( "raceStatus racing", \l -> raceStatus l { countdown = Nothing, finished = False } )
    , ( "raceStatus finished", \l -> raceStatus l { countdown = Nothing, finished = True } )
    , ( "slotLabel", \l -> slotLabel l 2 )
    , ( "slotOpen", \l -> slotOpen l 2 )
    , ( "you", you )
    , ( "finishedBadge", finishedBadge )
    , ( "editShipButton", editShipButton )
    , ( "buildOwnShip", buildOwnShip )
    , ( "crewHeading", crewHeading )
    , ( "twoPlayerHostDesc", twoPlayerHostDesc )
    , ( "gridHostDesc", gridHostDesc )
    , ( "joinRaceDesc", joinRaceDesc )
    ]


{-| Button that opens "Your ship" in the ship editor again.
-}
editShipButton : Language -> String
editShipButton lang =
    pick lang "Muokkaa laivaa" "Edit ship"


{-| The shipyard's builder button, when there is no ship of your own to edit.
-}
buildOwnShip : Language -> String
buildOwnShip lang =
    pick lang "Rakenna oma laiva" "Build your own ship"


{-| Heading of the shipyard's online play cards.
-}
crewHeading : Language -> String
crewHeading lang =
    pick lang "Purjehdi yhdessä" "Sail together"


twoPlayerHostDesc : Language -> String
twoPlayerHostDesc lang =
    pick lang "Avaa huone kahdelle kapteenille." "Open a room for two captains."


gridHostDesc : Language -> String
gridHostDesc lang =
    pick lang "Avaa huone jopa kahdeksalle laivalle." "Open a room for up to eight ships."


joinRaceDesc : Language -> String
joinRaceDesc lang =
    pick lang "Syötä kaverin huoneen koodi tai liitä hänen linkkinsä." "Enter a friend's room code or paste their link."
