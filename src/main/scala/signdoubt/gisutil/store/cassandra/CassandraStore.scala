package signdoubt.gisutil.store.cassandra

import com.datastax.driver.core.{Cluster, Row, Session}
import signdoubt.gisutil.EasyConfig
import signdoubt.gisutil.core.{BitArray64, SubSpace, ZValue}
import signdoubt.gisutil.store.SortedStore

/**
  * Sample implementation
  *
  * @param session session
  * @param mapper  mapper
  * @param range   range
  * @tparam T      value type
  */
class CassandraStore[T](session: Session, mapper: RowMapper[T], range: ScanRange[ZValue])
  extends SortedStore[ZValue, T] {
  override def from(from: ZValue, inclusive: Boolean): SortedStore[ZValue, T] =
    newStore(range.setFloor(from, inclusive))

  override def to(to: ZValue): SortedStore[ZValue, T] = newStore(range.setCeil(to, inclusive))

  override def until(until: ZValue): SortedStore[ZValue, T] = newStore(range.setCeil(until, !inclusive))

  private def newStore(nextRange: ScanRange[ZValue]) = new CassandraStore(session, mapper, nextRange)

  override def reverse(): SortedStore[ZValue, T] = newStore(range.reverse())

  override def floorEntry(key: ZValue): Option[(ZValue, T)] = {
    val rs = session.execute(
      "SELECT * FROM " + mapper.tableName + " WHERE row_key = 0 AND z_value <= ? ORDER BY z_value DESC;",
      new java.lang.Long(key))
    val itr = rs.iterator()
    if (itr.hasNext) {
      val row = itr.next()
      Some(mapper.toObject(row))
    } else {
      None
    }
  }

  override def ceilingEntry(key: ZValue): Option[(ZValue, T)] = throw new UnsupportedOperationException

  override def get(key: ZValue): Option[T] = {
    val rs = session.execute("SELECT * FROM " + mapper.tableName + " WHERE row_key = 0 AND z_value = ?;",
      new java.lang.Long(key))
    val itr = rs.iterator()
    if (itr.hasNext) {
      val row = itr.next()
      Some(mapper.toObject(row)._2)
    } else {
      None
    }
  }

  override def put(key: ZValue, value: T): Unit = mapper.insert(session, key, value)

  override def delete(key: ZValue): Unit = throw new UnsupportedOperationException

  override def toIterable: Iterable[(ZValue, T)] = {
    val rs = session.execute("SELECT * FROM " + mapper.tableName +
      " WHERE row_key = 0 " + toWhereClause(range, "z_value") + ";")
    val itr: java.util.Iterator[Row] = rs.iterator()
    new Iterable[(ZValue, T)] {
      override def iterator: Iterator[(ZValue, T)] = new EntryIterator(itr, mapper)
    }
  }

  def toWhereClause(range: ScanRange[ZValue], columnName: String): String = {
    val desc = if (range.isReverse) " ORDER BY " + columnName + " DESC " else ""
    val floorClause = range.floor match {
      case Some(value) =>
        val op = if (range.floorInclusive) " >= " else " > "
        columnName + op + value
      case None => ""
    }
    val ceilClause = range.ceil match {
      case Some(value) =>
        val op = if (range.ceilInclusive) " <= " else " < "
        columnName + op + value
      case None => ""
    }
    if (floorClause.isEmpty && ceilClause.isEmpty) {
      desc
    } else if (floorClause.nonEmpty && ceilClause.nonEmpty) {
      " AND " + floorClause + " AND " + ceilClause + desc
    } else {
      " AND " + floorClause + ceilClause + desc
    }
  }

  class EntryIterator(itr: java.util.Iterator[Row], mapper: RowMapper[T]) extends Iterator[(ZValue, T)] {
    override def hasNext: Boolean = itr.hasNext

    override def next(): (ZValue, T) = {
      val row = itr.next()
      mapper.toObject(row)
    }
  }

}

object CassandraStore {
  def createCluster(config: EasyConfig = EasyConfig()): Cluster = Cluster.builder()
    .withClusterName(config.get(CassandraConfig.clusterName))
    .addContactPoint(config.get(CassandraConfig.contactPoint))
    .build()

  def createStore(session: Session): Unit = {
    session.execute("CREATE SCHEMA simplex WITH REPLICATION = {'class':'SimpleStrategy', 'replication_factor': 1};")
    session.execute(
      """CREATE TABLE simplex.buckets(
        |  row_key INT,
        |  z_value BIGINT,
        |  info TEXT,
        |  PRIMARY KEY (row_key, z_value)
        |) WITH CLUSTERING ORDER BY (z_value ASC);
      """.stripMargin)

    session.execute(
      """CREATE TABLE simplex.sub_space_index(
        |  row_key INT,
        |  z_value BIGINT,
        |  bit_length INT,
        |  estimated_count INT,
        |  PRIMARY KEY (row_key, z_value)
        |) WITH CLUSTERING ORDER BY (z_value ASC);
      """.stripMargin)

  }

  def dropStore(session: Session): Unit = session.execute("DROP SCHEMA simplex;")

  def dataStore(session: Session): SortedStore[ZValue, String] =
    new CassandraStore[String](session, new StringMapper, new ScanRange[ZValue])

  def indexStore(session: Session): SortedStore[ZValue, SubSpace] =
    new CassandraStore[SubSpace](session, new SubSpaceMapper, new ScanRange[ZValue])
}

trait RowMapper[T] {
  val tableName: String

  def insert(session: Session, key: ZValue, value: T): Unit

  def toObject(row: Row): (ZValue, T)
}

class StringMapper extends RowMapper[String] {
  override val tableName: String = "simplex.buckets"

  override def insert(session: Session, key: ZValue, value: String): Unit =
    session.execute("INSERT INTO " + tableName + " (row_key, z_value, info) values (0, ?, ?);",
      new java.lang.Long(key), value)

  override def toObject(row: Row): (ZValue, String) = (row.getLong(1), row.getString(2))
}

class SubSpaceMapper extends RowMapper[SubSpace] {
  override val tableName: String = "simplex.sub_space_index"

  override def insert(session: Session, key: ZValue, value: SubSpace): Unit =
    session.execute("INSERT INTO " + tableName + "(row_key, z_value, bit_length, estimated_count) values (0, ?, ?, ?);",
      new java.lang.Long(key),
      new java.lang.Integer(value.spaceName.bitLength),
      new java.lang.Integer(value.estimatedCount))

  override def toObject(row: Row): (ZValue, SubSpace) = {
    val spaceId = row.getLong(1)
    val bitLength = row.getInt(2)
    val estimatedCount = row.getInt(3)
    (spaceId, SubSpace(BitArray64(spaceId, bitLength), estimatedCount))
  }
}

case class ScanRange[T](
  floor: Option[T] = None,
  floorInclusive: Boolean = true,
  ceil: Option[T] = None,
  ceilInclusive: Boolean = false,
  isReverse: Boolean = false
) {
  def setFloor(value: T, inclusive: Boolean): ScanRange[T] = ScanRange(Option(value), inclusive, ceil, ceilInclusive)

  def setCeil(value: T, inclusive: Boolean): ScanRange[T] = ScanRange(floor, floorInclusive, Option(value), inclusive)

  def reverse(): ScanRange[T] = ScanRange(floor, floorInclusive, ceil, ceilInclusive, !isReverse)
}
