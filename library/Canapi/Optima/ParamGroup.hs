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

cors :: ParamGroup Bool
cors =
  True <$ member "cors" (flag "Enable simple cors") <|>
  pure True

settings :: ParamGroup env -> ParamGroup (Word16, Bool, env)
settings env = (,,) <$> port <*> cors <*> subgroup "env" env
