module Main where

import Prelude hiding (delete, get, put, head)
import Main.HCurrying
import Canapi
import Fx


main = error "TODO"

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
          put validatedRpcSchemaSourceReceiver mempty (uncurryH . putRpcSchemaHandler),
          get (error "TODO") (error "TODO"),
          at "artifacts" (mconcat [
            get (error "TODO") (error "TODO"),
            by languageSegmentParser (mconcat [
              get artifactsRenderer (uncurryH getLanguageArtifacts)
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

artifactsRenderer :: Renderer Artifacts
artifactsRenderer = error "TODO"

putRpcSchemaHandler :: ByteString -> Name -> Name -> Fx env Err ()
putRpcSchemaHandler = error "TODO"

getLanguageArtifacts :: Language -> Name -> Name -> Fx Env Err Artifacts
getLanguageArtifacts = error "TODO"
