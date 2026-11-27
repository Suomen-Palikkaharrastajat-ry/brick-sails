module Sails.I18n exposing
    ( Lang
    , allPhrases
    , anchorDroppedToast
    , anchorWeighedToast
    , backToBoats
    , backToCourse
    , bestLine
    , biomeName
    , blueCoatsName
    , boatDescription
    , boatName
    , buildOwnShip
    , byChance
    , citadelName
    , coinsGained
    , coinsLabel
    , coinsScoreLabel
    , controlsHint
    , copyLink
    , copyRoomLink
    , countdownHint
    , courseTab
    , crewHeading
    , crewHeadline
    , crewTimeLabel
    , crewWaiting
    , dropAnchor
    , dropAnchorHint
    , editSeed
    , editShipButton
    , exitRace
    , factionBoardingToast
    , factionName
    , factionSunkToast
    , finalTimeLabel
    , finishToast
    , finishedBadge
    , fireButton
    , fireTitle
    , fromFlag
    , ghostShipAppearedToast
    , ghostShipBanishedToast
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
    , mapPickerLabels
    , mapTitle
    , mapWeatherName
    , mooringHint
    , muteLabel
    , newMapButton
    , newPersonalBest
    , nextGoal
    , nextHarbourTitle
    , overpoweredTitle
    , personalBestLine
    , personalBestShort
    , pirateSunkToast
    , piratesBoardedToast
    , piratesName
    , piratesSunkToast
    , playerCount
    , pointOfSailText
    , raceStatus
    , redCoatsName
    , reefScrapeToast
    , reefWarning
    , restartRace
    , rollWorld
    , roomCodeLabel
    , roomCodePlaceholder
    , roomLabel
    , sailAgain
    , sailLevelName
    , sailLevelTitle
    , sailStep
    , sailingStat
    , seasonName
    , setSail
    , shipLoadError
    , shipwrecked
    , shipwreckedText
    , shootHint
    , slotLabel
    , slotOpen
    , spaceKey
    , speedKnotsTitle
    , speedLabel
    , standardDroppedToast
    , standardRepatriatedToast
    , standardSeizedToast
    , starsAria
    , startRace
    , steerLabel
    , subtitle
    , title
    , toCode
    , townName
    , treasureStat
    , treasureToast
    , turnLabel
    , twoPlayerHost
    , twoPlayerHostDesc
    , weatherName
    , weighAnchor
    , windDirection
    , windLabel
    , worldTab
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

import Game.MapParams exposing (Biome(..), MapParams, Season(..), WeatherKind(..))
import Game.MapPicker as MapPicker
import I18n exposing (Language(..))
import Sails.Types exposing (Faction(..))


{-| Alias for Language.
-}
type alias Lang =
    Language



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
    -- The game name is the same in both languages, so it is not a translated phrase.
    pick lang "Laivaseikkailu" "Laivaseikkailu"


subtitle : Language -> String
subtitle lang =
    pick lang "Leluveneiden meriseikkailu" "Toy boat sea adventure"


{-| Name of a map biome (the same words as Brick Racer's, as a map code
means the same map in both games).
-}
biomeName : Language -> Biome -> String
biomeName lang biome =
    case biome of
        Farmland ->
            pick lang "Maaseutu" "Farmland"

        Boreal ->
            pick lang "Havumetsä" "Boreal"

        Fell ->
            pick lang "Tunturi" "Fell"

        Desert ->
            pick lang "Aavikko" "Desert"


{-| Name of a map season.
-}
seasonName : Language -> Season -> String
seasonName lang season =
    case season of
        Spring ->
            pick lang "Kevät" "Spring"

        Summer ->
            pick lang "Kesä" "Summer"

        Autumn ->
            pick lang "Syksy" "Autumn"

        Winter ->
            pick lang "Talvi" "Winter"


{-| Name of a map's weather.
-}
mapWeatherName : Language -> WeatherKind -> String
mapWeatherName lang weather =
    case weather of
        Clear ->
            pick lang "Selkeä" "Clear"

        Overcast ->
            pick lang "Pilvinen" "Overcast"

        Rain ->
            pick lang "Sade" "Rain"

        Storm ->
            pick lang "Ukkonen" "Storm"

        Fog ->
            pick lang "Sumu" "Fog"

        Snow ->
            pick lang "Lumi" "Snow"


{-| The course title of a map, such as "Boreal · Winter · Snow #48213".
-}
mapTitle : Language -> MapParams -> String
mapTitle lang map =
    String.join " · " [ biomeName lang map.biome, seasonName lang map.season, mapWeatherName lang map.weather ]
        ++ " #"
        ++ String.fromInt map.seed


{-| The words of the shared map picker.
-}
mapPickerLabels : Language -> MapPicker.Labels
mapPickerLabels lang =
    { biome = pick lang "Maasto" "Biome"
    , season = pick lang "Vuodenaika" "Season"
    , weather = pick lang "Sää" "Weather"
    , seed = pick lang "Siemen" "Seed"
    , random = pick lang "Arvo" "Random"
    , previous = pick lang "Edellinen kartta" "Previous map"
    , any = pick lang "Mikä vain" "Any"
    , code = pick lang "Karttakoodi (sama Pienajoissa)" "Map code (same in Brick Racer)"
    , biomeName = biomeName lang
    , seasonName = seasonName lang
    , weatherName = mapWeatherName lang
    }


{-| Results button that rolls a new seed with the same picks.
-}
newMapButton : Language -> String
newMapButton lang =
    pick lang "Uusi kartta" "New map"


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


{-| Best coins line of the Shipyard, with its stars.
-}
personalBestLine : Language -> Int -> Int -> String
personalBestLine lang coins starCount =
    pick lang "Paras " "Best " ++ coinCount lang coins ++ " · " ++ stars starCount


{-| Goal line of the Shipyard while the course has no best run yet: the
coins for 3 stars.
-}
bestLine : Language -> Int -> String
bestLine lang target =
    pick lang "3 tähteä: " "3 stars at " ++ coinCount lang target


{-| A number of gold coins, such as "25 kolikkoa" or "25 coins".
-}
coinCount : Language -> Int -> String
coinCount lang n =
    String.fromInt n
        ++ (if n == 1 then
                pick lang " kolikko" " coin"

            else
                pick lang " kolikkoa" " coins"
           )


{-| Coins just won, after a toast: "+10 kolikkoa" or "+10 coins".
-}
coinsGained : Language -> Int -> String
coinsGained lang n =
    "+" ++ coinCount lang n


{-| Name of the coin count of the status panel (its tooltip).
-}
coinsLabel : Language -> String
coinsLabel lang =
    pick lang "Kultakolikot" "Gold coins"


{-| Label of the score, the coins won, on the finish screen.
-}
coinsScoreLabel : Language -> String
coinsScoreLabel lang =
    pick lang "Kultaa saaliina" "Gold won"


controlsHint : Language -> Bool -> String
controlsHint lang tilt =
    if tilt then
        pick lang
            "Kallista puhelinta ohjataksesi. ⇑ ⇓ nostaa ja laskee purjetta. Napauta tai klikkaa ampuaksesi, kun olet lähellä kelluvia maalitauluja ja merirosvolaivoja, kapteeni."
            "Tilt the phone to steer. ⇑ ⇓ raise and lower the sail. Tap to shoot when you're near the floating targets or pirate ships, captain."

    else
        pick lang
            "Pidä ◀ ▶ (tai nuolinäppäimiä) pohjassa ohjataksesi. Nosta / laske purjetta ⇑ ⇓ (W / S). Napauta tai klikkaa ampuaksesi (välilyönti) kelluvien maalitaulujen ja merirosvolaivojen lähellä."
            "Hold ◀ ▶ (or arrow keys) to steer. Raise / lower sail with ⇑ ⇓ (W / S). Tap or click (Space) to shoot nearby floating targets and pirate ships."


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


{-| What tapping or clicking the sea does, as a hint: "Tap to shoot" on a
touch screen, "Click to shoot" with a mouse.
-}
shootHint : Language -> Bool -> String
shootHint lang touch =
    if touch then
        pick lang "Napauta ampuaksesi" "Tap to shoot"

    else
        pick lang "Klikkaa ampuaksesi" "Click to shoot"


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
        "Purjehdi tuulen mukana jokaiseen satamaan vuorollaan: hidasta hehkuvan renkaan sisällä ja kiinnity. Upota merirosvoja, löydä aarteita ja kerää kultakolikoita."
        "Sail with the wind to each harbour in turn: slow down inside the glowing ring to moor. Sink pirates, find treasure and win gold coins."


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


{-| Drop anchor button label and tooltip.
-}
dropAnchor : Language -> String
dropAnchor lang =
    pick lang "Laske ankkuri" "Drop anchor"


{-| Weigh anchor button label and tooltip.
-}
weighAnchor : Language -> String
weighAnchor lang =
    pick lang "Nosta ankkuri" "Weigh anchor"


{-| Toast when the anchor is dropped.
-}
anchorDroppedToast : Language -> String
anchorDroppedToast lang =
    pick lang "Ankkuri laskettu!" "Anchor dropped!"


{-| Toast when the anchor is weighed.
-}
anchorWeighedToast : Language -> String
anchorWeighedToast lang =
    pick lang "Ankkuri nostettu!" "Anchor weighed!"


{-| Mooring prompt when in harbour ready to drop anchor.
-}
dropAnchorHint : Language -> String
dropAnchorHint lang =
    pick lang "Laske ankkuri" "Drop anchor"


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


{-| Label of the time sailed on the finish screen (shown, not ranked).
-}
finalTimeLabel : Language -> String
finalTimeLabel lang =
    pick lang "Aika" "Time"


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


{-| A treasure chest taken aboard, with its coins.
-}
treasureToast : Language -> Int -> String
treasureToast lang coins =
    pick lang "Aarre! " "Treasure! " ++ coinsGained lang coins


finishToast : Language -> String
finishToast lang =
    pick lang "SATAMASSA!" "LAND HO!"


{-| A faction ship came alongside and boarded the boat.
-}
factionBoardingToast : Faction -> Lang -> String
factionBoardingToast faction lang =
    case faction of
        Pirates ->
            pick lang "Merirosvot kimppuun!" "Pirates boarded!"

        BlueCoats ->
            pick lang "Sinitakit kimppuun!" "BlueCoats boarded!"

        RedCoats ->
            pick lang "Punitakit kimppuun!" "RedCoats boarded!"


{-| A cannon ball sank a faction patrol ship.
-}
factionSunkToast : Faction -> Lang -> String
factionSunkToast faction lang =
    case faction of
        Pirates ->
            pick lang "Merirosvo upposi!" "Pirate sunk!"

        BlueCoats ->
            pick lang "Sinitakki upposi!" "BlueCoat sunk!"

        RedCoats ->
            pick lang "Punitakki upposi!" "RedCoat sunk!"


{-| A player seized an enemy faction's regalia standard from their harbor.
-}
standardSeizedToast : Faction -> Lang -> String
standardSeizedToast faction lang =
    case faction of
        Pirates ->
            pick lang "Merirosvojen lippu vallattu! Suuntaa kotisatamaan!" "Seized the Pirate Standard! Head for home port!"

        BlueCoats ->
            pick lang "Kuvernöörin vaakuna vallattu! Suuntaa kotisatamaan!" "Seized the Governor's Golden Crest! Head for home port!"

        RedCoats ->
            pick lang "Kuninkaallinen sotalippu vallattu! Suuntaa kotisatamaan!" "Seized the Crown's Royal Ensign! Head for home port!"


{-| A carried standard fell into the sea upon ship destruction or wreck.
-}
standardDroppedToast : Faction -> Lang -> String
standardDroppedToast faction lang =
    case faction of
        Pirates ->
            pick lang "Merirosvojen lippu putosi aaltoihin!" "Pirate Standard lost at sea!"

        BlueCoats ->
            pick lang "Kuvernöörin vaakuna putosi aaltoihin!" "Governor's Crest lost at sea!"

        RedCoats ->
            pick lang "Kuninkaallinen sotalippu putosi aaltoihin!" "Royal Ensign lost at sea!"


{-| A seized standard was successfully returned and repatriated to home citadel.
-}
standardRepatriatedToast : Faction -> Lang -> String
standardRepatriatedToast faction lang =
    case faction of
        Pirates ->
            pick lang "Merirosvojen lippu kotiutettu!" "Pirate Standard repatriated!"

        BlueCoats ->
            pick lang "Kuvernöörin vaakuna kotiutettu!" "Governor's Crest repatriated!"

        RedCoats ->
            pick lang "Kuninkaallinen sotalippu kotiutettu!" "Royal Ensign repatriated!"


{-| A warning when a deep keel scrapes a shallow coral reef.
-}
reefScrapeToast : Lang -> String
reefScrapeToast lang =
    pick lang "Riutta raapii köliä!" "The reef scrapes your keel!"


{-| The spectral ghost ship emerges from the squall mist.
-}
ghostShipAppearedToast : Lang -> String
ghostShipAppearedToast lang =
    pick lang "Aavelaiva nousee sumusta!" "The Ghost Ship rises from the mist!"


{-| The ghost ship was hit by cannon fire and banished into the spirit realm.
-}
ghostShipBanishedToast : Lang -> String
ghostShipBanishedToast lang =
    pick lang "Aavelaiva karkotettu!" "Ghost Ship banished!"


{-| A pirate ship came alongside and boarded the boat.
Kept for backwards compatibility.
-}
piratesBoardedToast : Language -> String
piratesBoardedToast lang =
    factionBoardingToast Pirates lang


{-| A cannon ball sank a pirate ship, with the coins it won.
Kept for backwards compatibility.
-}
pirateSunkToast : Language -> Int -> String
pirateSunkToast lang coins =
    factionSunkToast Pirates lang ++ " " ++ coinsGained lang coins


{-| Alias for pirateSunkToast. Kept for backwards compatibility.
-}
piratesSunkToast : Language -> Int -> String
piratesSunkToast =
    pirateSunkToast


harbourToast : Language -> Int -> Int -> String
harbourToast lang n total =
    pick lang "Satama " "Harbour " ++ String.fromInt n ++ "/" ++ String.fromInt total


{-| A cannon ball hit a floating target (it wins no coins).
-}
hitToast : Language -> String
hitToast lang =
    pick lang "Osuma!" "Hit!"


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


{-| The coins for 3 stars, on the finish screen.
-}
nextGoal : Language -> Int -> String
nextGoal lang target =
    pick lang "3 tähteä: " "3 stars at " ++ coinCount lang target


newPersonalBest : Language -> String
newPersonalBest lang =
    pick lang "Uusi oma ennätys!" "New personal best!"


{-| The best coins so far, on the finish screen.
-}
personalBestShort : Language -> Int -> String
personalBestShort lang coins =
    pick lang "Paras: " "Best: " ++ coinCount lang coins



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
                pick lang "KOKO MIEHISTÖ SATAMASSA" "WHOLE CREW IN PORT"

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


{-| Headline of a room's finish screen while crewmates are still at sea:
how many ships of the crew are in port.
-}
crewWaiting : Language -> Int -> Int -> String
crewWaiting lang inPort total =
    pick lang "Odotetaan miehistöä " "Waiting for the crew "
        ++ String.fromInt inPort
        ++ "/"
        ++ String.fromInt total


{-| Headline of a room's finish screen once the whole crew is in port, by
the crew's stars.
-}
crewHeadline : Language -> Int -> String
crewHeadline lang starCount =
    case starCount of
        3 ->
            pick lang "Merirosvokuninkaan miehistö!" "A pirate king's crew!"

        2 ->
            pick lang "Hienoa yhteispurjehdusta!" "Fine sailing together!"

        _ ->
            pick lang "Koko miehistö satamassa!" "The whole crew made port!"


{-| Under the crew's time: what it is, and how many ships are in port.
-}
crewTimeLabel : Language -> Int -> Int -> String
crewTimeLabel lang inPort total =
    pick lang "Miehistön aika · satamassa " "Crew time · in port "
        ++ String.fromInt inPort
        ++ "/"
        ++ String.fromInt total



-- FACTIONS & STRONGHOLDS


{-| Shown name of a faction in the War of the Three Ensigns.
-}
factionName : Language -> Faction -> String
factionName lang faction =
    case faction of
        Pirates ->
            piratesName lang

        BlueCoats ->
            blueCoatsName lang

        RedCoats ->
            redCoatsName lang


{-| Pirate faction name.
-}
piratesName : Language -> String
piratesName lang =
    pick lang "Merirosvot" "Pirates"


{-| BlueCoats faction name.
-}
blueCoatsName : Language -> String
blueCoatsName lang =
    pick lang "Sinitakit" "BlueCoats"


{-| RedCoats faction name.
-}
redCoatsName : Language -> String
redCoatsName lang =
    pick lang "Punitakit" "RedCoats"


{-| Shown name of each faction's stronghold citadel.
-}
citadelName : Language -> Faction -> String
citadelName lang faction =
    case faction of
        Pirates ->
            pick lang "Salakuljettajan poukama" "Smuggler's Reach"

        BlueCoats ->
            pick lang "Kuvernöörin ankkuripaikka" "Governor's Anchorage"

        RedCoats ->
            pick lang "Port Royal Palikka" "Port Royal Brick"


{-| Shown name of an archipelago settlement or harbor town.
-}
townName : Language -> String -> String
townName lang name =
    case ( lang, name ) of
        ( Fi, "Smuggler's Reach" ) ->
            "Salakuljettajan poukama"

        ( En, "Smuggler's Reach" ) ->
            "Smuggler's Reach"

        ( Fi, "Governor's Anchorage" ) ->
            "Kuvernöörin ankkuripaikka"

        ( En, "Governor's Anchorage" ) ->
            "Governor's Anchorage"

        ( Fi, "Port Royal Brick" ) ->
            "Port Royal Palikka"

        ( En, "Port Royal Brick" ) ->
            "Port Royal Brick"

        ( Fi, "Tortuga Cove" ) ->
            "Tortugan poukama"

        ( En, "Tortuga Cove" ) ->
            "Tortuga Cove"

        _ ->
            name



-- CHECKS


{-| Every phrase of the interface in both languages, by key, so a test can
check that none is missing (see `tests/LanguageTest.elm`).
-}
allPhrases : List ( String, Language -> String )
allPhrases =
    [ ( "subtitle", subtitle )
    , ( "biome Desert", \l -> biomeName l Desert )
    , ( "season Winter", \l -> seasonName l Winter )
    , ( "map weather Snow", \l -> mapWeatherName l Snow )
    , ( "newMapButton", newMapButton )
    , ( "faction Pirates", \l -> factionName l Pirates )
    , ( "faction BlueCoats", \l -> factionName l BlueCoats )
    , ( "faction RedCoats", \l -> factionName l RedCoats )
    , ( "citadel Pirates", \l -> citadelName l Pirates )
    , ( "citadel BlueCoats", \l -> citadelName l BlueCoats )
    , ( "citadel RedCoats", \l -> citadelName l RedCoats )
    , ( "town Tortuga Cove", \l -> townName l "Tortuga Cove" )
    , ( "town Smuggler's Reach", \l -> townName l "Smuggler's Reach" )
    , ( "town Governor's Anchorage", \l -> townName l "Governor's Anchorage" )
    , ( "town Port Royal Brick", \l -> townName l "Port Royal Brick" )
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
    , ( "personalBestLine", \l -> personalBestLine l 60 2 )
    , ( "bestLine", \l -> bestLine l 75 )
    , ( "coinsGained", \l -> coinsGained l 10 )
    , ( "coinsLabel", coinsLabel )
    , ( "coinsScoreLabel", coinsScoreLabel )
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
    , ( "shootHint tap", \l -> shootHint l True )
    , ( "shootHint click", \l -> shootHint l False )
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
    , ( "dropAnchor", dropAnchor )
    , ( "weighAnchor", weighAnchor )
    , ( "anchorDroppedToast", anchorDroppedToast )
    , ( "anchorWeighedToast", anchorWeighedToast )
    , ( "dropAnchorHint", dropAnchorHint )
    , ( "backToCourse", backToCourse )
    , ( "shipwrecked", shipwrecked )
    , ( "shipwreckedText", shipwreckedText )
    , ( "sailAgain", sailAgain )
    , ( "mainMenuButton", mainMenuButton )
    , ( "piratesBoardedToast", piratesBoardedToast )
    , ( "pirateSunkToast", \l -> pirateSunkToast l 2 )
    , ( "piratesSunkToast", \l -> piratesSunkToast l 2 )
    , ( "factionBoardingToast Pirates", factionBoardingToast Pirates )
    , ( "factionBoardingToast BlueCoats", factionBoardingToast BlueCoats )
    , ( "factionBoardingToast RedCoats", factionBoardingToast RedCoats )
    , ( "factionSunkToast Pirates", factionSunkToast Pirates )
    , ( "factionSunkToast BlueCoats", factionSunkToast BlueCoats )
    , ( "factionSunkToast RedCoats", factionSunkToast RedCoats )
    , ( "standardSeizedToast Pirates", standardSeizedToast Pirates )
    , ( "standardSeizedToast BlueCoats", standardSeizedToast BlueCoats )
    , ( "standardSeizedToast RedCoats", standardSeizedToast RedCoats )
    , ( "standardDroppedToast Pirates", standardDroppedToast Pirates )
    , ( "standardDroppedToast BlueCoats", standardDroppedToast BlueCoats )
    , ( "standardDroppedToast RedCoats", standardDroppedToast RedCoats )
    , ( "standardRepatriatedToast Pirates", standardRepatriatedToast Pirates )
    , ( "standardRepatriatedToast BlueCoats", standardRepatriatedToast BlueCoats )
    , ( "standardRepatriatedToast RedCoats", standardRepatriatedToast RedCoats )
    , ( "reefScrapeToast", reefScrapeToast )
    , ( "ghostShipAppearedToast", ghostShipAppearedToast )
    , ( "ghostShipBanishedToast", ghostShipBanishedToast )
    , ( "finalTimeLabel", finalTimeLabel )
    , ( "sailingStat", sailingStat )
    , ( "hitsStat", hitsStat )
    , ( "treasureStat", treasureStat )
    , ( "treasureToast", \l -> treasureToast l 25 )
    , ( "finishToast", finishToast )
    , ( "harbourToast", \l -> harbourToast l 1 7 )
    , ( "hitToast", hitToast )
    , ( "hullDamagedToast", \l -> hullDamagedToast l 80 )
    , ( "headline 3", \l -> headline l 3 )
    , ( "headline 2", \l -> headline l 2 )
    , ( "headline 1", \l -> headline l 1 )
    , ( "starsAria", \l -> starsAria l 2 )
    , ( "nextGoal", \l -> nextGoal l 75 )
    , ( "newPersonalBest", newPersonalBest )
    , ( "personalBestShort", \l -> personalBestShort l 60 )
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
    , ( "crewWaiting", \l -> crewWaiting l 1 2 )
    , ( "crewHeadline 3", \l -> crewHeadline l 3 )
    , ( "crewHeadline 2", \l -> crewHeadline l 2 )
    , ( "crewHeadline 1", \l -> crewHeadline l 1 )
    , ( "crewTimeLabel", \l -> crewTimeLabel l 2 2 )
    , ( "editShipButton", editShipButton )
    , ( "buildOwnShip", buildOwnShip )
    , ( "crewHeading", crewHeading )
    , ( "twoPlayerHostDesc", twoPlayerHostDesc )
    , ( "gridHostDesc", gridHostDesc )
    , ( "joinRaceDesc", joinRaceDesc )
    , ( "courseTab", courseTab )
    , ( "worldTab", worldTab )
    , ( "rollWorld", rollWorld )
    , ( "byChance", byChance )
    , ( "windLabel", \l -> windLabel l 5.4 )
    , ( "editSeed", editSeed )
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
    pick lang "Kaksi laivaa, yksi yhteinen seikkailu." "Two ships, one voyage together."


gridHostDesc : Language -> String
gridHostDesc lang =
    pick lang "Jopa kahdeksan laivaa, yksi yhteinen seikkailu." "Up to eight ships, one voyage together."


joinRaceDesc : Language -> String
joinRaceDesc lang =
    pick lang "Syötä kaverin huoneen koodi tai liitä hänen linkkinsä." "Enter a friend's room code or paste their link."


{-| The shipyard menu's two panels: the course, and the world it is on.
-}
courseTab : Language -> String
courseTab lang =
    pick lang "Rata" "Course"


worldTab : Language -> String
worldTab lang =
    pick lang "Maailma" "World"


{-| The dice button: a new seed with every pick left to chance.
-}
rollWorld : Language -> String
rollWorld lang =
    pick lang "Arvo uusi maailma" "Roll a new world"


{-| Said of a world setting that is left to chance (decided by the seed).
-}
byChance : Language -> String
byChance lang =
    pick lang "sattuman mukaan" "by chance"


{-| The world's wind, which follows from the biome, season and weather.
-}
windLabel : Language -> Float -> String
windLabel lang speed =
    pick lang "Tuuli " "Wind " ++ String.fromInt (round speed) ++ " m/s"


{-| Tooltip of the seed number, which opens the seed field.
-}
editSeed : Language -> String
editSeed lang =
    pick lang "Muuta siementä tai liitä karttakoodi" "Change the seed or paste a map code"
