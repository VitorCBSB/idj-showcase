module Types.GameInfo
    ( GameInfo
    , Award
    , Category(..)
    , decodeGameInfo
    ) where

import Json.Decode as Json exposing ((:=))

type alias GameInfo =
    { id : Int
    , year : Int
    , title : String
    , description : String
    , awards : List Award
    }

type alias Award = (Int, Category)

type Category =
    Game
    | Art
    | Music
    | Programming

decodeGameInfo : Json.Decoder GameInfo
decodeGameInfo = 
    let make id year title description awards =
        GameInfo id year title description awards in
    Json.object5 make
        ("id" := Json.int)
        ("year" := Json.int)
        ("title" := Json.string)
        ("description" := Json.string)
        ("awards" := Json.list decodeAward)

decodeAward : Json.Decoder Award
decodeAward =
    let make place category =
        (place, category) in
    Json.tuple2 make Json.int (Json.string `Json.andThen` categoryInfo)

categoryInfo : String -> Json.Decoder Category
categoryInfo catString =
    case catString of
        "Game" -> Json.succeed Game
        "Art" -> Json.succeed Art
        "Music" -> Json.succeed Music
        "Programming" -> Json.succeed Programming
        _ -> Json.fail ("Unrecognized category while decoding GameInfo JSON: " ++ catString)

