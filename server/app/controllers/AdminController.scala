package controllers

import dao.{AccountDao, UserDao}
import javax.inject.Inject
import models.Tables._
import org.joda.time.DateTime
import play.api.libs.json.Json
import play.api.mvc._
import tool.Tool
import utils.Utils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Created by yz on 2018/7/17
  */
class AdminController @Inject()(cc: ControllerComponents, formTool: FormTool, accountDao: AccountDao,
                                userDao: UserDao,tool:Tool) extends AbstractController(cc) {

  def userManageBefore = Action { implicit request =>
    Ok(views.html.admin.userManage())
  }

  def getAllUser = Action.async { implicit request =>
    getAllUserAction
  }

  def getAllUserAction = {
    userDao.selectAll.map { x =>
      val array = Utils.getArrayByTs(x)
      Ok(Json.toJson(array))
    }
  }

  def userNameCheck = Action.async { implicit request =>
    val data = formTool.userNameForm.bindFromRequest.get
    userDao.selectByName(data.name).zip(accountDao.selectById1).map { case (optionUser, admin) =>
      optionUser match {
        case Some(y) => Ok(Json.obj("valid" -> false))
        case None =>
          val valid = if (data.name == admin.account) false else true
          Ok(Json.obj("valid" -> valid))
      }
    }
  }

  def addUser = Action.async { implicit request =>
    val data = formTool.userForm.bindFromRequest().get
    val row = UserRow(0, data.name, data.password, new DateTime())
    userDao.insert(row).flatMap { x =>
      getAllUserAction
    }
  }

  def deleteUserById = Action.async { implicit request =>
    val data = formTool.idForm.bindFromRequest().get
    userDao.deleteById(data.id).flatMap { x =>
      Future{
        val userIdDir=tool.getUserIdDir(data.id)
        Utils.deleteDirectory(userIdDir)
      }
      getAllUserAction
    }
  }

  def getUserById = Action.async { implicit request =>
    val data = formTool.idForm.bindFromRequest().get
    userDao.selectById(data.id).map { x =>
      Ok(Utils.getJsonByT(x))
    }
  }

  def updateUser = Action.async { implicit request =>
    val data = formTool.userForm.bindFromRequest().get
    userDao.update(data).flatMap { x =>
      getAllUserAction
    }

  }

  def logout = Action { implicit request =>
    Redirect(routes.UserController.loginBefore()).flashing("info" -> "退出登录成功!").removingFromSession("admin")
  }

  def changePasswordBefore = Action { implicit request =>
    Ok(views.html.admin.changePassword())
  }

  def changePassword = Action.async { implicit request =>
    val data = formTool.changePasswordForm.bindFromRequest().get
    accountDao.selectById1.flatMap { x =>
      if (data.password == x.password) {
        val row = AccountRow(x.id, x.account, data.newPassword)
        accountDao.update(row).map { y =>
          Redirect(routes.UserController.loginBefore()).flashing("info" -> "密码修改成功!").removingFromSession("admin")
        }
      } else {
        Future.successful(Redirect(routes.AdminController.changePasswordBefore()).flashing("info" -> "用户名或密码错误!"))
      }
    }
  }


}
