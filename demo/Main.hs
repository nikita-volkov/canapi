module Main where

import Canapi
import Main.HCurrying
import Prelude hiding (delete, get, head, put)

main = error "TODO"

data Env

data Language

data Artifacts

data Name

root :: Env -> Resource ()
root env =
  mconcat
    [ at "groups" mempty,
      at
        "rpc"
        ( mconcat
            [ by
                nameSegmentParser
                ( mconcat
                    [ by
                        nameSegmentParser
                        ( mconcat
                            [ put validatedRpcSchemaSourceReceiver mempty (uncurryH . putRpcSchemaHandler),
                              get (error "TODO") (error "TODO"),
                              at
                                "artifacts"
                                ( mconcat
                                    [ get (error "TODO") (error "TODO"),
                                      by
                                        languageSegmentParser
                                        ( mconcat
                                            [ get artifactsRenderer (uncurryH (getLanguageArtifacts env))
                                            ]
                                        )
                                    ]
                                )
                            ]
                        )
                    ]
                )
            ]
        )
    ]

nameSegmentParser :: SegmentParser Name
nameSegmentParser = error "TODO"

languageSegmentParser :: SegmentParser Language
languageSegmentParser = error "TODO"

validatedRpcSchemaSourceReceiver :: Receiver ByteString
validatedRpcSchemaSourceReceiver = error "TODO"

artifactsRenderer :: Renderer Artifacts
artifactsRenderer = error "TODO"

putRpcSchemaHandler :: ByteString -> Name -> Name -> IO (Either Err ())
putRpcSchemaHandler = error "TODO"

getLanguageArtifacts :: Env -> Language -> Name -> Name -> IO (Either Err Artifacts)
getLanguageArtifacts = error "TODO"
