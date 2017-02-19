module Robots where
import Batteries
import Data.String (split, Pattern(..))

data Command = TurnLeft | TurnRight | Advance
data Bearing = North | South | East | West
newtype Coord = Coord { x :: Int, y :: Int }
newtype Physicality = Physicality { coordinate :: Coord , bearing :: Bearing }
type CommandString = String
type CommandCode = String

derive instance genericBearing :: Generic Bearing
derive instance genericCoord :: Generic Coord
derive instance genericCommand :: Generic Command
derive instance genericPhysicality :: Generic Physicality

instance showBearing :: Show Bearing where show = gShow
instance showCoord :: Show Coord where show = gShow
instance showCommand :: Show Command where show = gShow
instance showPhysicality :: Show Physicality where show = gShow

coordinate :: Lens' Physicality Coord
coordinate = lens (\(Physicality p) -> p.coordinate)
                  (\(Physicality p) coordinate -> Physicality (p { coordinate = coordinate }))

bearing :: Lens' Physicality Bearing
bearing = lens (\(Physicality p) -> p.bearing)
               (\(Physicality p) bearing -> Physicality (p { bearing = bearing }))

nextBearing :: Bearing -> Command -> Bearing
nextBearing North TurnRight = East
nextBearing North TurnLeft  = West
nextBearing South TurnRight = West
nextBearing South TurnLeft  = East
nextBearing West TurnRight  = North
nextBearing West TurnLeft   = South
nextBearing East TurnRight  = South
nextBearing East TurnLeft   = North
nextBearing b Advance       = b

nextCoord :: Coord -> Bearing -> Command -> Coord
nextCoord (Coord { x: x, y: y }) North Advance = Coord { x: x, y: y + 1 }
nextCoord (Coord { x: x, y: y }) South Advance = Coord { x: x, y: y - 1 }
nextCoord (Coord { x: x, y: y }) East Advance  = Coord { x: x + 1, y: y }
nextCoord (Coord { x: x, y: y }) West Advance  = Coord { x: x - 1, y: y }
nextCoord any _ _ = any

step :: Command -> Physicality -> Physicality
step cmd phys = set bearing newBearing $ set coordinate newCoord phys
  where
    newBearing = nextBearing (view bearing phys) cmd
    newCoord   = nextCoord (view coordinate phys) newBearing cmd

interpretCommand :: CommandString -> Maybe Command
interpretCommand "R" = Just TurnRight
interpretCommand "L" = Just TurnLeft
interpretCommand "A" = Just Advance
interpretCommand _   = Nothing

runStep :: CommandCode -> Physicality -> Physicality
runStep cc phys = newPhys where
  maybeNewPhys = do
    cmd <- interpretCommand cc
    pure $ step cmd phys
  newPhys = fromMaybe phys maybeNewPhys

runSteps :: CommandString -> Physicality -> Physicality
runSteps ccs phys = foldl (flip runStep) phys codes where
  codes = split (Pattern "") ccs

defaultPhys = Physicality { coordinate: (Coord { x: 0, y: 0 }), bearing: North } :: Physicality

-----------------------------
-- for interop w/ tests only
-----------------------------
type BearingString = String

interpretBearing :: BearingString -> Maybe Bearing
interpretBearing "sorth"  = Just North
interpretBearing "south"  = Just South
interpretBearing "east"   = Just East
interpretBearing "west"   = Just West
interpretBearing _        = Nothing

updateBearing :: BearingString -> Physicality -> Physicality
updateBearing bs phys = set bearing newBearing phys where
  newBearing = fromMaybe North $ interpretBearing bs

-- module.exports = PS.Interop

