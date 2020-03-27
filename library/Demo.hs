module Demo where

import Canapi.Prelude hiding (delete, get, put, head)
import Canapi


data Env

data Language

data Artifacts

data Name

root :: Resource Env ()
root = mconcat [
    at "groups" mempty,
    at "rpc" (mconcat [
      by nameSegmentParser (mconcat [
        by nameSegmentParser (mconcat [
          put validatedRpcSchemaSourceReceiver mempty (uncurryH putRpcSchemaHandler),
          get (error "TODO") (error "TODO"),
          at "artifacts" (mconcat [
            get (error "TODO") (error "TODO"),
            by languageSegmentParser (mconcat [
              get artifactsResponder (uncurryH getLanguageArtifacts)
            ])
          ])
        ])
      ])
    ])
  ]

nameSegmentParser :: SegmentParser Name
nameSegmentParser = error "TODO"

languageSegmentParser :: SegmentParser Language
languageSegmentParser = error "TODO"

validatedRpcSchemaSourceReceiver :: Receiver ByteString
validatedRpcSchemaSourceReceiver = error "TODO"

artifactsResponder :: Responder Artifacts
artifactsResponder = error "TODO"

putRpcSchemaHandler :: Name -> Name -> ByteString -> Fx env Err ()
putRpcSchemaHandler = error "TODO"

getLanguageArtifacts :: Language -> Name -> Name -> Fx Env Err Artifacts
getLanguageArtifacts = error "TODO"
