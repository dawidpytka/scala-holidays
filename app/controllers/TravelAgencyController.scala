package controllers

import models.Offer

import javax.inject.{Inject, Singleton}
import org.joda.time.DateTime
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents, Result}
import services.TravelAgencyService

import scala.collection.mutable.ListBuffer


@Singleton
class TravelAgencyController @Inject()(val controllerComponents: ControllerComponents, service: TravelAgencyService) extends BaseController{
  private var offers = ListBuffer[Offer]()

  def getOffers: Action[AnyContent] = Action { implicit request =>
    offers = service.getBestOffers(new DateTime().plusDays(60), new DateTime().plusDays(70), List("Czarnogóra", "Grecja"), 2)
//    val bestOffers: Unit = service.getBestOffers(new DateTime().plusDays(60), new DateTime().plusDays(70), List("Czarnogóra", "Grecja"), 2)
    val r: Result = Ok( views.html.offers(offers))
    r
  }
}
