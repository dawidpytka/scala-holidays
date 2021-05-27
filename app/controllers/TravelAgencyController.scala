package controllers

import javax.inject.{Inject, Singleton}
import org.joda.time.DateTime
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents, Result}
import services.TravelAgencyService


@Singleton
class TravelAgencyController @Inject()(val controllerComponents: ControllerComponents, service: TravelAgencyService) extends BaseController{

  def getOffers: Action[AnyContent] = Action { implicit request =>
//    val bestOffers: Unit = service.getBestOffers(new DateTime().plusDays(60), new DateTime().plusDays(70), List("Czarnogóra", "Grecja"), 2)
    val r: Result = Ok( service.getBestOffers(new DateTime().plusDays(60), new DateTime().plusDays(70), List("Czarnogóra", "Grecja"), 2).toString)
    r
  }
}
