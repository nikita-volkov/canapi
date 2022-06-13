module Main where

import Canapi
import Main.HCurrying
import Prelude hiding (delete, get, head, put)

main = error "TODO"

data Env

data Language

data Artifacts

data Name

root :: Env -> [Resource ()]
root env =
  [ at "groups" [],
    at "rpc" $
      [ by nameSegment $
          [ by nameSegment $
              [ put validatedRpcSchemaSourceReceiver mempty (uncurryH . putRpcSchemaHandler),
                get (error "TODO") (error "TODO"),
                at "artifacts" $
                  [ get (error "TODO") (error "TODO"),
                    by languageSegment $
                      [ get artifactsRenderer (uncurryH (getLanguageArtifacts env))
                      ]
                  ]
              ]
          ]
      ]
  ]

nameSegment :: Segment Name
nameSegment = error "TODO"

languageSegment :: Segment Language
languageSegment = error "TODO"

validatedRpcSchemaSourceReceiver :: Receiver ByteString
validatedRpcSchemaSourceReceiver = error "TODO"

artifactsRenderer :: Renderer Artifacts
artifactsRenderer = error "TODO"

putRpcSchemaHandler :: ByteString -> Name -> Name -> IO (Either Err ())
putRpcSchemaHandler = error "TODO"

getLanguageArtifacts :: Env -> Language -> Name -> Name -> IO (Either Err Artifacts)
getLanguageArtifacts = error "TODO"
