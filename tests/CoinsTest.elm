module CoinsTest exposing (suite)

{-| Gold coins, the score of a voyage (`Sails.Coins`): each of the four
sources (a pirate sunk, a boarding fought off, a treasure chest, a harbour
docked) adds its coins, a sunk ship throws a burst of coins, the ending,
the stars and the records rank by coins, and no time penalty or "-2 s" is
left.
-}

import Expect
import I18n exposing (Language(..))
import Math.Vector3 as Vec3
import Pages.Sail as Sail exposing (SailModel, SailMsg(..))
import Sails.Archipelago as Archipelago
import Sails.Boats as Boats
import Sails.Coins as Coins
import Sails.Ending as Ending
import Sails.I18n as T
import Sails.Physics as Physics
import Sails.Pirates as Pirates exposing (Pirate)
import Sails.Records as Records
import Sails.Types exposing (Course, LeaderboardEntry)
import Test exposing (Test, describe, test)


course : Course
course =
    Archipelago.defaultCourse


fresh : SailModel
fresh =
    Sail.initSail En Boats.default course True Nothing Nothing { available = False, enabled = False } 800 600


frame : Float
frame =
    1 / 60


step : SailModel -> SailModel
step m =
    Tuple.first (Sail.update (Tick (frame * 1000)) m)


{-| Sail on for a little over 3 s (the race starts at once).
-}
racing : SailModel -> SailModel
racing m =
    List.foldl (\_ x -> step x) m (List.range 1 200)


firstPirate : SailModel -> (Pirate -> Expect.Expectation) -> Expect.Expectation
firstPirate m expect =
    case m.pirates of
        p :: _ ->
            expect p

        [] ->
            Expect.fail "no pirates"


{-| The frame in which a cannon ball comes down on the pirate.
-}
sinking : Pirate -> SailModel -> SailModel
sinking p m =
    let
        at =
            (Pirates.stateAt (m.simTime + frame) p).position

        ball =
            { id = 99, sourceSlot = 1, position = Vec3.add at (Vec3.vec3 0 1.5 0), velocity = Vec3.vec3 0 0 0, age = 0 }
    in
    step { m | cannonballs = [ ball ] }


entry : String -> Int -> Float -> LeaderboardEntry
entry courseId coins time =
    { courseId = courseId, player = "You", coins = coins, timeSeconds = time, isoDate = "", vehicleName = "Sloop" }


key : String
key =
    "g1:boreal-winter-snow-48213"


suite : Test
suite =
    describe "Gold coins"
        [ describe "the coins of each source"
            [ test "the values: pirate sunk 10, boarded 15, treasure chest 25, harbour 5" <|
                \_ ->
                    Expect.equal [ 10, 15, 25, 5 ]
                        [ Coins.pirateSunk, Coins.pirateBoarded, Coins.treasureChest, Coins.harbourDocked ]
            , test "the coin target counts on sinking the whole fleet" <|
                \_ -> Expect.equal Pirates.fleetSize Coins.piratesToSink
            , test "docking at a harbour adds its coins, by anchor or by mooring" <|
                \_ ->
                    let
                        start =
                            Physics.initProgress course

                        ( anchored, _ ) =
                            Physics.dropAnchorProgress start

                        harbour =
                            List.head course.waypoints

                        moored =
                            case harbour of
                                Just wp ->
                                    let
                                        s =
                                            Physics.initSailingState course
                                    in
                                    Tuple.first (Physics.stepProgress (Physics.dockSeconds + 0.1) { s | position = wp.position, speed = 0 } course.waypoints start)

                                Nothing ->
                                    start
                    in
                    Expect.equal ( 0, Coins.harbourDocked, Coins.harbourDocked ) ( start.coins, anchored.coins, moored.coins )
            , test "sinking a pirate adds its coins" <|
                \_ ->
                    firstPirate fresh
                        (\p ->
                            let
                                m =
                                    racing fresh
                            in
                            Expect.equal (m.progress.coins + Coins.pirateSunk) (sinking p m).progress.coins
                        )
            , test "a pirate boarding the boat adds the boarding coins" <|
                \_ ->
                    firstPirate fresh
                        (\p ->
                            let
                                m =
                                    racing fresh

                                s =
                                    m.state

                                alongside =
                                    { m | state = { s | position = (Pirates.stateAt (m.simTime + frame) p).position, speed = 0 } }

                                boarded =
                                    step alongside
                            in
                            Expect.equal ( True, m.progress.coins + Coins.pirateBoarded ) ( boarded.pirateCooldown > 0, boarded.progress.coins )
                        )
            , test "taking a treasure chest aboard adds its coins and pops coins over the boat" <|
                \_ ->
                    case course.treasures of
                        t :: _ ->
                            let
                                m =
                                    racing fresh

                                s =
                                    m.state

                                found =
                                    step { m | state = { s | position = t.position, speed = 0 }, pirates = [] }
                            in
                            Expect.equal ( [ t.id ], m.progress.coins + Coins.treasureChest, True )
                                ( found.progress.treasureFound, found.progress.coins, List.length found.flyingCoins >= Coins.popCount )

                        [] ->
                            Expect.fail "no treasure"
            ]
        , describe "flying coins"
            [ test "a sunk ship throws a burst of coins, at most 12" <|
                \_ ->
                    firstPirate fresh
                        (\p ->
                            let
                                n =
                                    List.length (sinking p (racing fresh)).flyingCoins
                            in
                            Expect.all [ \_ -> Expect.greaterThan 0 n, \_ -> Expect.atMost 12 n ] ()
                        )
            , test "the coins fly up, come down and are gone after their lifetime" <|
                \_ ->
                    let
                        coins =
                            Coins.burst 1 (Vec3.vec3 0 0 0)

                        later t =
                            List.foldl (\_ c -> Coins.step frame c) coins (List.range 1 (round (t / frame)))

                        maxY cs =
                            List.map (.position >> Vec3.getY) cs |> List.maximum |> Maybe.withDefault 0
                    in
                    Expect.all
                        [ \_ -> Expect.equal Coins.burstCount (List.length coins)
                        , \_ -> Expect.greaterThan (maxY coins) (maxY (later 0.4))
                        , \_ -> Expect.equal [] (later (Coins.lifetime + 0.1))
                        ]
                        ()
            ]
        , describe "ranking by coins"
            [ test "the ending's score is the coins and its stars follow the coin target" <|
                \_ ->
                    let
                        p0 =
                            Physics.initProgress course

                        target =
                            Coins.courseTarget course

                        endingFor coins seconds =
                            Ending.start { coinTarget = target, personalBest = Nothing } { p0 | coins = coins, elapsedSeconds = seconds, finished = True }
                    in
                    Expect.all
                        [ \_ -> Expect.equal ( target, 3 ) (endingFor target 500 |> (\e -> ( e.score, e.stars )))
                        , \_ -> Expect.equal 2 (endingFor (target // 2 + 1) 10).stars
                        , \_ -> Expect.equal 1 (endingFor 5 10).stars
                        , -- A fast run with few coins is not better than a slow rich one.
                          \_ -> Expect.equal True ((endingFor 5 1).stars < (endingFor target 900).stars)
                        ]
                        ()
            , test "only more coins than the personal best is a new record, whatever the time" <|
                \_ ->
                    let
                        p0 =
                            Physics.initProgress course

                        run =
                            { p0 | coins = 40, elapsedSeconds = 999, finished = True }

                        new best =
                            (Ending.start { coinTarget = 75, personalBest = Just best } run).isNewRecord
                    in
                    Expect.equal [ True, False, False ] [ new 39, new 40, new 60 ]
            , test "the best run is the one with the most coins, the faster of a tie" <|
                \_ ->
                    Records.empty
                        |> Records.recordRun (entry key 30 50)
                        |> Records.recordRun (entry key 60 120)
                        |> Records.recordRun (entry key 60 100)
                        |> Records.bestForCourse key
                        |> Maybe.map (\e -> ( e.coins, e.timeSeconds ))
                        |> Expect.equal (Just ( 60, 100 ))
            , test "records round-trip through JSON with their coins" <|
                \_ ->
                    let
                        board =
                            Records.recordRun (entry key 45 80) Records.empty
                    in
                    Expect.equal board (Records.fromJson (Records.toJson board))
            , test "old time-only records are left out and do not break the board" <|
                \_ ->
                    let
                        json =
                            """{"entries":[{"courseId":"g1:boreal-winter-snow-48213","player":"You","timeSeconds":42.5,"isoDate":"","vehicleName":"Sloop"},{"courseId":"g1:boreal-winter-snow-48213","player":"You","coins":20,"timeSeconds":90,"isoDate":"","vehicleName":"Sloop"}]}"""
                    in
                    Expect.equal [ 20 ] (List.map .coins (Records.fromJson json).entries)
            , test "a board that does not read starts empty" <|
                \_ -> Expect.equal Records.empty (Records.fromJson "[]")
            ]
        , describe "no time penalty"
            [ test "sinking a pirate shows its coins, not seconds off" <|
                \_ ->
                    firstPirate fresh
                        (\p ->
                            case (sinking p (racing fresh)).toast of
                                Just ( toast, _ ) ->
                                    Expect.equal ( True, False, False )
                                        ( String.contains (T.coinsGained En Coins.pirateSunk) toast
                                        , String.endsWith " s" toast
                                        , String.contains "−" toast
                                        )

                                Nothing ->
                                    Expect.fail "no toast"
                        )
            , test "the hit, treasure and sunk phrases have no -2 s or seconds in them" <|
                \_ ->
                    [ T.hitToast En, T.hitToast Fi, T.treasureToast En 25, T.treasureToast Fi 25, T.pirateSunkToast En 10, T.countdownHint En, T.countdownHint Fi ]
                        |> List.filter (\s -> String.contains "2 s" s || String.contains "−" s || String.contains " s)" s)
                        |> Expect.equal []
            ]
        ]
