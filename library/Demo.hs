module Demo where

import Canapi.Prelude hiding (delete, get, put, head)
import Canapi


data Env

data Language

data Artifacts

data Name

root :: [Resource Env ()]
root = [
    at "groups" [],
    at "rpc" [
      by [nameParser] [
        by [nameParser] [
          put [validatedRpcSchemaSourceReceiver] [] (uncurryH putRpcSchemaHandler),
          get (error "TODO") (error "TODO"),
          at "artifacts" [
            get (error "TODO") (error "TODO"),
            by [languageParser] [
              get [artifactsResponder] (uncurryH getLanguageArtifacts)
            ]
          ]
        ]
      ]
    ]
  ]

nameParser :: SegmentParser Name
nameParser = error "TODO"

languageParser :: SegmentParser Language
languageParser = error "TODO"

validatedRpcSchemaSourceReceiver :: Receiver ByteString
validatedRpcSchemaSourceReceiver = error "TODO"

artifactsResponder :: Responder Artifacts
artifactsResponder = error "TODO"

putRpcSchemaHandler :: Name -> Name -> ByteString -> Fx env Err ()
putRpcSchemaHandler = error "TODO"

getLanguageArtifacts :: Language -> Name -> Name -> Fx Env Err Artifacts
getLanguageArtifacts = error "TODO"
