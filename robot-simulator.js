const R = require('./robots')

// I need an "imperative shell" to wrap the functional-goodness!
//
// I _could probably_ model the mutation w/ certain constructs (Control.Monad.ST / Control.Monad.Eff.Ref)
// but it seemed a lot easier to let JavaScript be the mutable-ball-of-state it wants to be in order to let the tests pass.
//
// I probably could have eliminated some of this mapping but I started from
// the written requirements rather than the JS specs and only realized I needed this file afterwards. se la vie

class Robot {
  constructor() {
    this.physicality = {}
  }

  get bearing() {
    const theBearing = this.physicality.bearing
    return this.directionFrom(theBearing)
  }

  get coordinates() {
    const coord = this.physicality.coordinate
    return [coord.x, coord.y]
  }

  orient(bearing) {
    this.physicality = R.updateBearing(bearing)(this.physicality)
  }

  at(x, y) {
    this.physicality = R.updateCoord(x)(y)(this.physicality)
  }

  turnRight() {
    this.physicality = R.runStep('R')(this.physicality)
  }

  turnLeft() {
    this.physicality = R.runStep('L')(this.physicality)
  }

  advance() {
    this.physicality = R.runStep('A')(this.physicality)
  }

  commandStringFrom(command) {
    if (command instanceof R.TurnLeft) { return 'turnLeft' }
    if (command instanceof R.TurnRight) { return 'turnRight' }
    if (command instanceof R.Advance) { return 'advance' }
    return ''
  }

  instructions(command) {
    const results = R.interpretCommands(command)
    return results.map(this.commandStringFrom)
  }

  place({ x, y, direction }) {
    const coordinate = { x, y }
    const bearing = this.makeBearing(direction)
    const physicality = new R.Physicality({ coordinate, bearing })
    this.physicality = physicality
  }

  directionFrom(bearing) {
    if(bearing instanceof R.North) { return 'north' }
    if(bearing instanceof R.South) { return 'south' }
    if(bearing instanceof R.East) { return 'east' }
    if(bearing instanceof R.West) { return 'west' }
    return null
  }

  makeBearing(direction) {
    switch(direction) {
      case ('north'): return new R.North
      case ('south'): return new R.South
      case ('east'): return new R.East
      case ('west'): return new R.West
      default: return null
    }
  }

  evaluate(multiCommands) {
    this.physicality = R.runSteps(multiCommands)(this.physicality)
  }
}

module.exports = Robot
