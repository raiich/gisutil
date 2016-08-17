package signdoubt.gisutil

/**
  * This class is for easy accessible config.
  *
  * @param map config key and value mapping
  */
case class EasyConfig(map: Map[String, String] = Map[String, String]()) {
  /**
    * Returns config value for given config key.
    *
    * @param configKey config key
    * @tparam T value type to be casted
    * @return casted config value
    */
  def get[T](configKey: ConfigKey[T]): T = map.get(configKey.key).map(configKey.cast).getOrElse(configKey.defaultValue)
}

/**
  * Config key class for [[EasyConfig]].
  *
  * @param key          config key
  * @param defaultValue default value
  * @param cast         cast function
  * @tparam T casted type of config value
  */
case class ConfigKey[T](key: String, defaultValue: T, cast: String => T)

object ConfigKey {
  def apply(key: String, defaultValue: String): ConfigKey[String] = new ConfigKey(key, defaultValue, v => v)

  def apply(key: String, defaultValue: Int): ConfigKey[Int] = new ConfigKey(key, defaultValue, v => v.toInt)

  def apply(key: String, defaultValue: Double): ConfigKey[Double] = new ConfigKey(key, defaultValue, v => v.toDouble)
}
