package dao

import javax.inject.Inject
import models.Tables._
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Created by yz on 2018/4/27
  */
class AdjustMissionDao @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends
  HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  def insert(row: AdjustMissionRow): Future[Unit] = db.run(AdjustMission += row).map(_ => ())

  def selectByMissionName(userId: Int, missionName: String): Future[AdjustMissionRow] = db.run(AdjustMission.
    filter(_.userId === userId).filter(_.missionName === missionName).result.head)

  def selectOptionByMissionName(userId: Int, missionName: String): Future[Option[AdjustMissionRow]] = db.run(AdjustMission.
    filter(_.userId === userId).filter(_.missionName === missionName).result.headOption)

  def selectByMissionId(userId: Int, missionId: Int): Future[AdjustMissionRow] = db.run(AdjustMission.
    filter(_.userId === userId).filter(_.id === missionId).result.head)

  def update(row: AdjustMissionRow): Future[Unit] = db.run(AdjustMission.filter(_.id === row.id).update(row)).map(_ => ())

  def selectAll(userId: Int): Future[Seq[AdjustMissionRow]] = db.run(AdjustMission.filter(_.userId === userId).sortBy(_.id.desc).result)

  def selectAll(userId: Int, state: String): Future[Seq[AdjustMissionRow]] = db.run(AdjustMission.
    filter(x => x.userId === userId && x.state === state).sortBy(_.id.desc).result)

  def deleteById(id: Int): Future[Unit] = db.run(AdjustMission.filter(_.id === id).delete).map(_ => ())

  def deleteByUserId(userId: Int): Future[Unit] = db.run(AdjustMission.filter(_.userId === userId).delete).map(_ => ())


}
