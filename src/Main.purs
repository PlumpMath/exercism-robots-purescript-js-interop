module Main where

import Batteries (CONSOLE, Eff, Unit, logShow, bind, ($), pure)
import Robots (
  Bearing(..)
, Coord(..)
, Physicality(..)
, runSteps
, defaultPhys
, updateBearing
)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let phys = Physicality { coordinate: (Coord { x: 7, y: 3 }), bearing: North }
  let newPhys = runSteps "RAALAL" phys
  pure defaultPhys
  pure updateBearing
  logShow newPhys
