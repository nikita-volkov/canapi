module Canapi.Strelka.Optima.ParamGroup
(
  settings,
)
where

import Canapi.Prelude
import Canapi.Strelka.Types
import Optima


port :: ParamGroup Word16
port =
  member "port" $
  value "HTTP server port"
    (showable 8000)
    unformatted
    implicitlyParsed

settings :: ParamGroup (Managed env) -> ParamGroup (Word16, Managed env)
settings env = (,) <$> port <*> subgroup "env" env
