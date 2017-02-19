module Main where

import Batteries (CONSOLE, Eff, Unit, logShow, bind, pure)
import Robots (
  Bearing(..)
, Coord(..)
, Physicality(..)
, runSteps
-- The following are just needed so they can be used when compiled;
-- otherwise they get optimized out since they aren't used!
, defaultPhys
, updateBearing
, updateCoord
, interpretCommands
)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let phys = Physicality { coordinate: (Coord { x: 7, y: 3 }), bearing: North }
  let newPhys = runSteps "RAALAL" phys
  -- no-op "usage" to allow the functions to be exported;
  -- there's probably some way to compile w/ out having to do this
  pure defaultPhys
  pure updateCoord
  pure updateBearing
  pure interpretCommands
  logShow newPhys
