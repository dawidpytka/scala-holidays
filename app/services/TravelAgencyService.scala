package services

import javax.inject.Inject
import models.Offer
import org.joda.time.DateTime
import org.jsoup.Jsoup

import scala.collection.JavaConverters._

//Todo: get offers
//Todo: get countries
//Todo: get all data for filters
class TravelAgencyService @Inject() (offers: List[Offer]){
  def getRainbowOffers(dateFrom: DateTime, dateTo: DateTime, countries: List[String], numberOfPersons: Int): List[Offer] = {
    List()
  }

  def getItakaOffers(dateFrom: DateTime, dateTo: DateTime, countries: List[String], numberOfPersons: Int): List[Offer] = {
    List()
  }

  def getCoralTravelOffers(dateFrom: DateTime, dateTo: DateTime, countries: List[String], numberOfPersons: Int): List[Offer] = {
    List()
  }

  def getAllCounties(): List[String] = {
    val sourceUrl: String = "https://www.itaka.pl/nasze-kierunki/"
    val htmlDocument = Jsoup.connect(s"${sourceUrl}").get()
    val countryDomElements = htmlDocument.select(".fhotel_region_header .fhotel_region_name").asScala
    countryDomElements.map(_.text()).toList
  }

  def getNumbersOfPersons(): List[Int] = {
    (1 to 16).toList
  }

}
