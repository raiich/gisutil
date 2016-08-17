package signdoubt.gisutil.store.cassandra

import signdoubt.gisutil.ConfigKey

object CassandraConfig {
  val clusterName = ConfigKey("store.name", "defaultstore")
  val contactPoint = ConfigKey("contact.point", "127.0.0.1")
}
