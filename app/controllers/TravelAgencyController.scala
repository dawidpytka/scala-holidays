package controllers

import forms.SearchForm
import models.Offer
import services.TravelAgencyService

import play.api.data.Forms.{mapping, text}
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents, Result}
import play.api.data.{Form, Forms}
import play.api.data.format._
import play.api.data.Forms._
import play.api.i18n.I18nSupport

import javax.inject.Singleton
import javax.inject.Inject
import java.util.Date
import scala.collection.mutable.ListBuffer


@Singleton
class TravelAgencyController @Inject()(val controllerComponents: ControllerComponents, service: TravelAgencyService) extends BaseController with I18nSupport{
  private var countries = ListBuffer[String]()
  private var offers = ListBuffer[Offer]()
  val dateTimeLocal: Formatter[Date] = Formats.dateFormat("yyyy-MM-dd")
  private val searchForm = Form[SearchForm](
    mapping(
      "dateFrom" -> Forms.of(dateTimeLocal),
      "dateTo" -> Forms.of(dateTimeLocal),
          "minDaysAmount" -> number(min = 1, max = 30),
          "starsAmount" -> number(min = 1, max = 5),
              "personsAmount" -> number(min = 1, max = 12),
      "country" -> text,
      "isAllInclusive" -> boolean,
      "isLastMinute" -> boolean
    )
    (SearchForm.apply)(SearchForm.unapply))


  def getOffers: Action[AnyContent] = Action { implicit request =>
    offers = ListBuffer[Offer]()
    if(countries.isEmpty) countries = countries ++ service.getAllCounties
    Ok( views.html.offers(offers,searchForm, routes.TravelAgencyController.searchTrips,countries))
  }

  def searchTrips: Action[AnyContent] = Action {
    implicit request =>
      searchForm.bindFromRequest.fold(
        formWithErrors => {
          println(formWithErrors.errors.toString)
          if(countries.isEmpty) countries = countries ++ service.getAllCounties
          Ok( views.html.offers(offers,formWithErrors, routes.TravelAgencyController.searchTrips, countries ))
        },
        formCorrect => {
          offers = service.getBestOffers(formCorrect.dateFrom, formCorrect.dateTo, List(formCorrect.Country), formCorrect.personsAmount, formCorrect.minDaysAmount, formCorrect.starsAmount, formCorrect.isLastMinute, formCorrect.isAllInclusive )
          if(countries.isEmpty) countries = countries ++ service.getAllCounties
          Ok( views.html.offers(offers, searchForm, routes.TravelAgencyController.searchTrips, countries))
        }

      )
  }
}
