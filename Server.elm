port module Server exposing (main)

import Platform
import Json.Encode as Json
import Json.Decode as Decode
import Dict exposing (Dict)
import Debug

type alias Model =
  { chatLog : List ( String, String )
  , waitingId : List Id
  , readingFile : Dict Id String
  , host : String
  }

main =
  Platform.worker
    { init = init
    , update = update
    , subscriptions = subscriptions
    }

type alias Request =
  { method : String
  , headers : Json.Value
  , url : String
  , body : Maybe String
  }

type alias Response =
  { statusCode : Int
  , statusMessage : String
  , headers : Json.Value
  , body : String
  }



-- MODEL --

init : String -> ( Model, Cmd Msg )
init host =
  ( Model [] [] Dict.empty host
  , Cmd.none )



-- UPDATE --

type Msg
  = HttpRequest ( Json.Value, Id )
  | ReadResult ( Maybe String, Id )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    HttpRequest ( reqJson, id ) ->
      case reqJson |> Decode.decodeValue requestDecoder of
        Ok req ->
          case req.method of
            "GET" ->
              case req.url |> String.dropLeft 1 of
                "" ->
                  ( { model
                    | readingFile =
                        model.readingFile
                          |> Dict.insert id "index.html"
                    }
                  , readFile ( "index.html", id ) )
                path ->
                  if String.startsWith "get" path
                  then
                    if String.contains "force=true" path
                    then
                      ( model
                      , response ( chatResponse model.chatLog, id )
                      )
                    else
                      ( { model
                        | waitingId = id :: model.waitingId
                        }
                      , Cmd.none
                      )
                  else
                    ( { model
                      | readingFile =
                          model.readingFile
                            |> Dict.insert id path
                      }
                    , readFile ( path, id ) )
            "POST" ->
              case req.body |> Debug.log "body" of
                Just str ->
                  case String.split "|" str of
                    name::content ->
                      let
                        waitingId = model.waitingId
                        newModel =
                          { model
                          | chatLog =
                              model.chatLog ++ [(name, String.concat content)]
                          , waitingId = []
                          }
                        postResponse =
                          response ( Response 200 "OK" mimeStringUtf8 "", id )
                      in
                        ( newModel
                        , postResponse ::
                            ( waitingId
                                |> List.map
                                  (\wId ->
                                    response
                                      (chatResponse newModel.chatLog, wId)
                                  )
                            )
                            |> Cmd.batch
                        )
                    _ ->
                      ( model
                      , response ( errorResponse 400 "Bad Request", id )
                      )
                        |> Debug.log "1"
                _ ->
                  ( model
                  , response ( errorResponse 400 "Bad Request", id )
                  )
                  |> Debug.log "2"
            _ ->
              ( model
              , response ( errorResponse 400 "Bad Request", id )
              )
              |> Debug.log "3"

        Err _ ->
          ( model
          , response ( errorResponse 400 "Bad Request", id )
          )

    ReadResult ( result, id ) ->
      case result of
        Nothing ->
          ( { model
            | readingFile =
                model.readingFile
                  |> Dict.remove id
            }
          , response ( errorResponse 404 "Not Found", id )
          )

        Just str ->
          let
            newStr =
              case Dict.get id model.readingFile of
                Just "index.html" ->
                  let
                    hostStr = "\"" ++ model.host ++ "\""
                  in
                    String.replace "~host~" hostStr str
                _ -> str
          in
            ( { model
              | readingFile =
                  model.readingFile
                    |> Dict.remove id
              }
            , response ( Response 200 "OK" mimeHtmlUtf8 newStr, id )
            )

requestDecoder : Decode.Decoder Request
requestDecoder =
  Decode.map4
  Request
  (Decode.field "method" Decode.string)
  (Decode.field "headers" Decode.value)
  (Decode.field "url" Decode.string)
  (Decode.field "body" Decode.string |> Decode.maybe)

errorResponse : Int -> String -> Response
errorResponse statusCode reason =
  Response statusCode reason
  mimeHtmlUtf8
  (String.fromInt statusCode ++ " " ++ reason)

mimeHtmlUtf8 : Json.Value
mimeHtmlUtf8 =
  ( "Content-Type", Json.string "text/html; charset=UTF-8" )
    |> List.singleton
    |> Json.object

mimeStringUtf8 : Json.Value
mimeStringUtf8 =
  ( "Content-Type", Json.string "text/plane; charset=UTF-8" )
    |> List.singleton
    |> Json.object

chatResponse : List (String, String) -> Response
chatResponse logs =
  let
    logStr =
      logs
        |> List.map
          (\(name, content) -> name ++ " : " ++ content )
        |> String.join "\\n"
  in
    Response 200 "OK" mimeStringUtf8 logStr


-- SUBSCRIPTIONS --

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
  [ request HttpRequest
  , readResult ReadResult
  ]



-- PORTS --

type alias Id = Int

port request : (( Json.Value, Id ) -> msg) -> Sub msg
port response : ( Response, Id ) -> Cmd msg

port readFile : ( String, Id ) -> Cmd msg
port readResult : (( Maybe String, Id ) -> msg) -> Sub msg