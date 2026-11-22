module Sails.Boats exposing (all, byName, default)

{-| The four toy boats on offer. They differ only a little: a faster boat
turns wider, a nimbler one is a touch slower.
-}

import Math.Vector3 exposing (vec3)
import Sails.Types exposing (Boat, BoatKind(..))


default : Boat
default =
    sloop


all : List Boat
all =
    [ sloop, cutter, schooner, catamaran ]


byName : String -> Boat
byName name =
    all
        |> List.filter (\b -> b.name == name)
        |> List.head
        |> Maybe.withDefault default


sloop : Boat
sloop =
    { kind = Sloop
    , name = "Sloop"
    , description = "Balanced all-rounder."
    , hullColor = vec3 0.55 0.33 0.16
    , sailColor = vec3 0.97 0.96 0.9
    , topSpeed = 9.0
    , turnRate = 95.0
    }


cutter : Boat
cutter =
    { kind = Cutter
    , name = "Cutter"
    , description = "Nimble: turns on a stud."
    , hullColor = vec3 0.12 0.35 0.7
    , sailColor = vec3 0.98 0.85 0.3
    , topSpeed = 8.6
    , turnRate = 125.0
    }


schooner : Boat
schooner =
    { kind = Schooner
    , name = "Schooner"
    , description = "Two masts, quick on a reach."
    , hullColor = vec3 0.2 0.2 0.22
    , sailColor = vec3 0.85 0.15 0.15
    , topSpeed = 9.5
    , turnRate = 85.0
    }


catamaran : Boat
catamaran =
    { kind = Catamaran
    , name = "Catamaran"
    , description = "Fastest, but turns wide."
    , hullColor = vec3 0.95 0.95 0.95
    , sailColor = vec3 0.2 0.7 0.85
    , topSpeed = 10.0
    , turnRate = 72.0
    }
