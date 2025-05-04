package ru.otus.module4.homework.dao.repository

import io.getquill.context.ZioJdbc._
import ru.otus.module4.homework.dao.entity._
import ru.otus.module4.phoneBook.db
import zio.{ULayer, ZIO, ZLayer}

import java.sql.SQLException

trait UserRepository {
  def findUser(userId: UserId): QIO[Option[User]]

  def createUser(user: User): QIO[User]

  def createUsers(users: List[User]): QIO[List[User]]

  def updateUser(user: User): QIO[Unit]

  def deleteUser(user: User): QIO[Unit]

  def findByLastName(lastName: String): QIO[List[User]]

  def list(): QIO[List[User]]

  def userRoles(userId: UserId): QIO[List[Role]]

  def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit]

  def listUsersWithRole(roleCode: RoleCode): QIO[List[User]]

  def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]]
}


class UserRepositoryImpl extends UserRepository {
  val dc: db.Ctx.type = db.Ctx

  import dc._

  private val users = quote(querySchema[User]("users"))
  private val roles = quote(querySchema[Role]("roles"))
  private val userToRoles = quote(querySchema[UserToRole]("user_to_roles"))

  override def findUser(userId: UserId): QIO[Option[User]] =
    run(users.filter(_.id == lift(userId.id))).map(_.headOption)

  override def createUser(user: User): QIO[User] = {
    run {
      query[User].insertValue(lift(user)).returning(_.id)
    }.map(id => user.copy(id = id))
  }

  override def createUsers(users: List[User]): QIO[List[User]] = {
    transaction {
      ZIO.foreach(users)(createUser)
    }.refineOrDie {
      case e: SQLException => e
      case other => new SQLException(other.getMessage)
    }
  }

  override def updateUser(user: User): QIO[Unit] = {
    run {
      query[User]
        .filter(_.id == lift(user.id))
        .updateValue(lift(user))
    }.unit
  }

  override def deleteUser(user: User): QIO[Unit] =
    run(users.filter(_.id == lift(user.id)).delete).unit

  override def findByLastName(lastName: String): QIO[List[User]] =
    run(users.filter(_.lastName == lift(lastName)))

  override def list(): QIO[List[User]] =
    run(users)

  override def userRoles(userId: UserId): QIO[List[Role]] = {
    val q = quote {
      for {
        utr <- userToRoles if utr.userId == lift(userId.id)
        r <- roles if r.roleId == utr.roleId
      } yield r
    }
    run(q)
  }

  override def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit] = {
    // First get the role ID
    val roleIdQ = quote {
      roles.filter(_.code == lift(roleCode.code)).map(_.roleId)
    }

    // Then insert
    for {
      roleId <- run(roleIdQ).map(_.headOption.getOrElse(
        throw new RuntimeException(s"Role ${roleCode.code} not found")
      ))
      _ <- run(quote {
        userToRoles.insert(
          _.roleId -> lift(roleId),
          _.userId -> lift(userId.id)
        )
      })
    } yield ()
  }.orDie

  override def listUsersWithRole(roleCode: RoleCode): QIO[List[User]] = {
    val q = quote {
      for {
        r <- roles if r.code == lift(roleCode.code)
        utr <- userToRoles if utr.roleId == r.roleId
        u <- users if u.id == utr.userId
      } yield u
    }
    run(q)
  }

  override def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]] =
    run(roles.filter(_.code == lift(roleCode.code))).map(_.headOption)
}

object UserRepository {

  val layer: ULayer[UserRepository] = ZLayer.succeed(new UserRepositoryImpl)
}