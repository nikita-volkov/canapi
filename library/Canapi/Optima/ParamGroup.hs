module Canapi.Optima.ParamGroup
(
  settings,
)
where

import Canapi.Prelude
import Optima


port :: ParamGroup Word16
port =
  member "port" $
  value "HTTP server port"
    (showable 8000)
    unformatted
    implicitlyParsed

settings :: ParamGroup env -> ParamGroup (Word16, env)
settings env = (,) <$> port <*> subgroup "env" env
