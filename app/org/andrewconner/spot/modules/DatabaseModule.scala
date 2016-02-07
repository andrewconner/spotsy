package org.andrewconner.spot.modules

import com.typesafe.config.Config
import play.api.db.slick._
import play.api.db.{ConnectionPool, DBApi, HikariCPComponents, DBComponents}
import play.api.inject.{ ApplicationLifecycle, Injector }
import play.api.{ Application, Configuration, Environment }
import slick.backend.DatabaseConfig
import slick.driver.{ JdbcProfile, H2Driver }
import slick.jdbc.JdbcBackend
import slick.profile.BasicProfile

trait MyDbConfig extends DatabaseConfig[JdbcProfile]

trait DatabaseModule {
  def environment: Environment
  def configuration: Configuration
  def injector: Injector
  def applicationLifecycle: ApplicationLifecycle

  // Check comment above Slick's DatabaseFactoryDef.forConfig in JdbcBackend.scala
  // https://github.com/slick/slick/blob/3.1.0-RC2/slick/src/main/scala/slick/jdbc/JdbcBackend.scala#L119

  lazy val myDbConfig: MyDbConfig = new DbFactoryConfig(slickApi.dbConfig[JdbcProfile](DbName("mydb"))) with MyDbConfig {}

  private lazy val slickApi = new DefaultSlickApi(environment, configuration, applicationLifecycle)
}

private[this] abstract class DbFactoryConfig[T <: BasicProfile](cfg: DatabaseConfig[T]) extends DatabaseConfig[T] {
  val db = cfg.db
  val driver = cfg.driver
  val config = cfg.config
  val driverName = cfg.driverName
  val driverIsObject = cfg.driverIsObject
}
