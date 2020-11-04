import { doTest } from './Test.purs'
import { testRoofs } from './testroofplates'
import { testPanels } from './testpanels'

// convert test data to RoofPlate objects
const roofs = testRoofs.roofplates
const panels = testPanels.panels
// load the house and roofs
doTest(roofs)(panels)()
