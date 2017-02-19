const R = require('./robots')

class Robot {
  constructor() {
    this.physicality = {}
  }

  get bearing() {
    const theBearing = this.physicality.bearing
    if(theBearing instanceof R.North) { return 'north' }
    if(theBearing instanceof R.South) { return 'south' }
    if(theBearing instanceof R.East) { return 'east' }
    if(theBearing instanceof R.West) { return 'west' }
    return null
  }

  get coordinates() {

  }

  orient(bearing) {
    this.physicality = R.updateBearing(bearing)(this.physicality)
  }
}

module.exports = Robot
