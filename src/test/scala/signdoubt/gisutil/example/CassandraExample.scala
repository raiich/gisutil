package signdoubt.gisutil.example

import com.datastax.driver.core.{Cluster, Session}
import signdoubt.gisutil.core.{GisUtilConfig, SpatialQueryHandlerFactory}
import signdoubt.gisutil.store.cassandra.CassandraStore
import signdoubt.gisutil.{EasyConfig, Rectangle, RoundedPoint, SpatialQueryHandler}

import scala.util.Random

object CassandraExample {
  val diff = 151234567
  val longitudeBase = 1351234567 - diff
  val latitudeBase = 351234567 - diff
  val pointCount = 1000
  val readTrial = 4
  val config = EasyConfig(Map(GisUtilConfig.pointCountMax.key -> "3"))

  Random.setSeed(0)

  def main(args: Array[String]): Unit = {
    val cluster: Cluster = CassandraStore.createCluster(config)
    try {
      val session = cluster.connect()
      executeWithSession(session)
    } finally {
      cluster.close()
    }
  }

  def executeWithSession(session: Session): Unit = {
    try {
      CassandraStore.createStore(session)
      // initializes spatial query handler
      val handler: SpatialQueryHandler[String] = SpatialQueryHandlerFactory.subSpaceAwareQueryHandler(
        CassandraStore.dataStore(session), CassandraStore.indexStore(session), config)

      // puts points
      for (i <- 1 to pointCount) {
        val longitude = longitudeBase + nextInt
        val latitude = latitudeBase + nextInt
        val point = RoundedPoint(longitude, latitude)
        handler.put(point, point.toString)
      }

      // scans
      for (i <- 1 to readTrial) {
        val (longitudeFloor, longitudeCeil) = sort(longitudeBase + nextInt, longitudeBase + nextInt)
        val (latitudeFloor, latitudeCeil) = sort(longitudeBase + nextInt, longitudeBase + nextInt)
        val rectangle = Rectangle(longitudeFloor, latitudeFloor, longitudeCeil, latitudeCeil)
        val scanned = handler.scan(rectangle).toSeq
        println("size = " + scanned.size)
      }

      // k-nearest neighbor
      for (i <- 1 to readTrial) {
        val (longitude, latitude) = (longitudeBase + nextInt, longitudeBase + nextInt)
        val basePoint = RoundedPoint(longitude, latitude)
        val kNN = handler.nearestNeighbor(basePoint, 5)
        println("size = " + kNN.size)
      }
    } finally {
      CassandraStore.dropStore(session)
    }

  }

  def nextInt = Random.nextInt(diff * 2)

  def sort(a: Int, b: Int): (Int, Int) = if (a <= b) (a, b) else (b, a)
}
