package controllers

import javax.inject.{Inject, Singleton}
import org.jsoup.Jsoup
import play.api.mvc
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}
import play.mvc.Result

@Singleton
class TravelAgencyController @Inject()(val controllerComponents: ControllerComponents) extends BaseController{

  def getAll(): Action[AnyContent] = Action { implicit request =>
    val sourceUrl: String = "https://kwestiasmaku.com"
    val htmlDocument = Jsoup.connect(s"${sourceUrl}/home-przepisy?page=1").get()
    val r: mvc.Result = Ok(htmlDocument)
    r
  }
}
