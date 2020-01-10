package controllers

import dao.{AccountDao, UserDao}
import javax.inject.Inject
import models.Tables.UserRow
import play.api.mvc._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Created by yz on 2018/7/17
  */
class UserController @Inject()(cc: ControllerComponents, formTool: FormTool, userDao: UserDao,
                               accountDao: AccountDao) extends AbstractController(cc) {

  def loginBefore = Action { implicit request =>
    Ok(views.html.login())
  }

  def login = Action.async { implicit request =>
    val data = formTool.userForm.bindFromRequest().get
    accountDao.selectById1.zip(userDao.selectByUserData(data)).map { case (account, optionUser) =>
      if (data.name == account.account && data.password == account.password) {
        Redirect(routes.AdminController.userManageBefore()).addingToSession("admin" -> data.name)
      } else if (optionUser.isDefined) {
        val user = optionUser.get
        Redirect(routes.UserController.missionManageBefore()).addingToSession("user" -> data.name,
          "id" -> user.id.toString)
      } else {
        Redirect(routes.UserController.loginBefore()).flashing("info" -> "用户名或密码错误!")
      }
    }
  }

  def missionManageBefore = Action { implicit request =>
    Ok(views.html.user.missionManage())
  }

  def logout = Action { implicit request =>
    Redirect(routes.UserController.loginBefore()).flashing("info" -> "退出登录成功!").removingFromSession("user")
  }

  def changePasswordBefore = Action { implicit request =>
    Ok(views.html.user.changePassword())
  }

  def changePassword = Action.async { implicit request =>
    val data = formTool.changePasswordForm.bindFromRequest().get
    val name = request.session.get("user").get
    userDao.selectByName(name).flatMap { x =>
      val dbUser = x.get
      if (data.password == dbUser.password) {
        val row = UserData(name, data.newPassword)
        userDao.update(row).map { y =>
          Redirect(routes.UserController.loginBefore()).flashing("info" -> "密码修改成功!").removingFromSession("user")
        }
      } else {
        Future.successful(Redirect(routes.UserController.changePasswordBefore()).flashing("info" -> "用户名或密码错误!"))
      }
    }

  }


}
