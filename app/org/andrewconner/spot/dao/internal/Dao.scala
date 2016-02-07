package org.andrewconner.spot.dao.internal

import play.api.db.slick.DatabaseConfigProvider
import play.api.db.slick.HasDatabaseConfig
import slick.backend.DatabaseConfig
import slick.driver.JdbcProfile
import slick.profile.BasicProfile

trait Dao[P <: BasicProfile] extends HasDatabaseConfig[JdbcProfile] {

}

trait JdbcDao extends Dao[JdbcProfile] {
  protected val dbConfig: DatabaseConfig[JdbcProfile]
}
