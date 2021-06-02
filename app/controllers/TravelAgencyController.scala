package controllers



import forms.SearchForm
import models.Offer
import org.joda.time.DateTime
import play.api.data.Forms.{mapping, text}
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents, Result}
import play.api.data.{Form, Forms}
import play.api.data.validation.Constraints._

import scala.collection.mutable.ListBuffer
import play.api.data.format._
import services.TravelAgencyService
import javax.inject.Singleton
import java.util.Date
import javax.inject.Inject
import scala.collection.mutable.ListBuffer
import play.api.data.Forms._

import javax.inject.Inject

import play.api.i18n.I18nSupport

import play.api.i18n.MessagesApi


@Singleton
class TravelAgencyController @Inject()(val controllerComponents: ControllerComponents, service: TravelAgencyService) extends BaseController with I18nSupport{
  private var offers = ListBuffer[Offer]()
  val dateTimeLocal: Formatter[Date] = Formats.dateFormat("yyyy-MM-dd")
  private val searchForm = Form[SearchForm](
    mapping(
      "dateFrom" -> Forms.of(dateTimeLocal),
      "dateTo" -> Forms.of(dateTimeLocal),
          "minDaysAmount" -> number,
          "starsAmount" -> number,
              "personsAmount" -> number
    )
    (SearchForm.apply)(SearchForm.unapply))


  def getOffers: Action[AnyContent] = Action { implicit request =>
    offers = ListBuffer[Offer]()
    //offers = service.getBestOffers(new DateTime().plusDays(60), new DateTime().plusDays(70), List("Czarnogóra", "Grecja"), 2)
//    val bestOffers: Unit = service.getBestOffers(new DateTime().plusDays(60), new DateTime().plusDays(70), List("Czarnogóra", "Grecja"), 2)
    Ok( views.html.offers(offers,searchForm, routes.TravelAgencyController.searchTrips))

  }

  def searchTrips = Action {
    implicit request =>
      searchForm.bindFromRequest.fold(
        formWithErrors => {
          println("BUU")
          println(formWithErrors.errors.toString)
          //offers = service.getBestOffers(new DateTime().plusDays(60), new DateTime().plusDays(70), List("Czarnogóra", "Grecja"), 2)
          Ok( views.html.offers(offers,searchForm, routes.TravelAgencyController.searchTrips))
        },
        formCorrect => {
          println("yupi")
          offers = service.getBestOffers(new DateTime().plusDays(60), new DateTime().plusDays(70), List("Czarnogóra", "Grecja"), 2)
          Ok( views.html.offers(offers,searchForm, routes.TravelAgencyController.searchTrips))
        }

      )
  }
}
