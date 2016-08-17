package signdoubt.gisutil.core

import signdoubt.gisutil.EasyConfig

class SubSpaceAwareQueryHandlerSuite extends QueryHandlerSuite {
  def fixture = SpatialQueryHandlerFactory.subSpaceAwareQueryHandlerInMemory(
    EasyConfig(Map(GisUtilConfig.pointCountMax.key -> "3")))
}
