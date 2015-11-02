trait Teapot {

  def boilButton()
  val minVolumeInMilliliters: Int
  val maxVolumeInMilliliters: Int
  var celsiumTemperature: Int
  var currentVolumeInMilliliters: Int
}

trait BoilingServiceComp {
  val boiler: BoilingService
  trait BoilingService {
    def boil(teapot: Teapot)
  }
}

trait BoilingServiceCompImpl extends BoilingServiceComp {
  class BoilingServiceImpl extends BoilingService {
    def boil(teapot: Teapot) = {
      teapot.celsiumTemperature = 100
    }
  }
}

trait WaterCounterComp {
  val counter: WaterCounter
  trait WaterCounter {
    def countWater(teapot: Teapot): Int
  }
}

trait WaterCounterCompImpl extends WaterCounterComp {
  class WaterCounterImpl extends WaterCounter {
    def countWater(teapot: Teapot): Int = teapot.currentVolumeInMilliliters
  }
}

trait WaterCheckerComp {
  val checker: WaterChecker
  trait WaterChecker {
    def checkWater(teapot: Teapot): Boolean
  }
}

trait WaterCheckerCompImpl extends WaterCheckerComp {
  class WaterCheckerImpl extends WaterChecker {
    def checkWater(teapot: Teapot): Boolean =
      teapot.minVolumeInMilliliters <= teapot.currentVolumeInMilliliters &&
        teapot.currentVolumeInMilliliters <= teapot.maxVolumeInMilliliters
  }
}

trait HeatCheckerComp {
  val heatChecker: HeatChecker
  trait HeatChecker {
    def checkHeat(teapot: Teapot): Boolean
  }
}

trait HeatCheckerCompImpl extends HeatCheckerComp {
  class HeatCheckerImpl extends HeatChecker {
    def checkHeat(teapot: Teapot): Boolean = teapot.celsiumTemperature <= 98
  }
}

trait DummyTeapot extends Teapot {
  this: BoilingServiceComp with WaterCounterComp with WaterCheckerComp =>
  override def boilButton() = {
    if (checker.checkWater(this)) {
      boiler.boil(this)
    }
  }

  override val minVolumeInMilliliters = 500
  override val maxVolumeInMilliliters = 1500
}

trait SmartTeapot extends Teapot {
  this: BoilingServiceComp with WaterCounterComp with WaterCheckerComp with HeatCheckerComp =>
  override def boilButton() = {
    if (checker.checkWater(this) && heatChecker.checkHeat(this)) {
      boiler.boil(this)
    }
  }

  override val minVolumeInMilliliters = 200
  override val maxVolumeInMilliliters = 3000
}

object Teapot1 extends DummyTeapot
with WaterCounterCompImpl
with BoilingServiceCompImpl
with WaterCheckerCompImpl {
  override val boiler: BoilingService = new BoilingServiceImpl()
  override val checker: WaterChecker = new WaterCheckerImpl()
  override val counter: WaterCounter = new WaterCounterImpl()
  var currentVolumeInMilliliters = 750
  var celsiumTemperature = 20
}

object Teapot2 extends SmartTeapot
with WaterCounterCompImpl
with BoilingServiceCompImpl
with WaterCheckerCompImpl
with HeatCheckerCompImpl {
  override val boiler: BoilingService = new BoilingServiceImpl()
  override val checker: WaterChecker = new WaterCheckerImpl()
  override val counter: WaterCounter = new WaterCounterImpl()
  override val heatChecker: HeatChecker = new HeatCheckerImpl()
  var currentVolumeInMilliliters = 300
  var celsiumTemperature = 20
}

Teapot1.boilButton()
Teapot2.boilButton()
