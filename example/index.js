import { doTest } from './Test.purs'
import { testRoofs } from './testroofplates'

// convert test data to RoofPlate objects
const roofs = testRoofs.roofplates
// load the house and roofs
doTest(roofs)()
