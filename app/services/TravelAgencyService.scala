package services

import javax.inject.Inject
import models.Offer
import org.joda.time.DateTime
import org.jsoup.Jsoup

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer


class TravelAgencyService @Inject() (){

  def getBestOffers(dateFrom: DateTime, dateTo: DateTime, countries: List[String], numberOfPersons: Int = 1): ListBuffer[Offer] ={
    var bestOffers = new ListBuffer[Offer]()

    bestOffers = ListBuffer.concat(
      getRainbowOffers(dateFrom, dateTo, countries, numberOfPersons),
      getItakaOffers(dateFrom, dateTo, countries, numberOfPersons),
      getTraveliadaOffers(dateFrom, dateTo, countries, numberOfPersons))

    bestOffers = bestOffers.sortWith((o1,o2) => o1.price < o2.price)
    bestOffers.take(5)
  }

  def getRainbowOffers(dateFrom: DateTime, dateTo: DateTime, countries: List[String], numberOfPersons: Int): List[Offer] = {

    List()
  }

  private def getItakaOffers(dateFrom: DateTime, dateTo: DateTime, countries: List[String], numberOfPersons: Int = 1): List[Offer] = {
    var sourceUrl: String = "https://www.itaka.pl/wyniki-wyszukiwania/wakacje/?view=offerList"+"&adults="+numberOfPersons
    if (dateFrom!=null) sourceUrl = sourceUrl + "&date-from=" + dateFrom.toString("yyyy-MM-dd")
    if (dateTo!=null) sourceUrl = sourceUrl + "&date-to=" + dateTo.toString("yyyy-MM-dd")

    if (countries != null && countries.nonEmpty) {
      sourceUrl = sourceUrl + "&dest-region="
      for(country <- countries) yield sourceUrl = sourceUrl + changePolishSigns(country.toLowerCase) + "%2C"
      sourceUrl = sourceUrl.dropRight(3)
    }
    sourceUrl = sourceUrl + "&order=priceAsc"

    val htmlDocument = Jsoup.connect(sourceUrl).get()
    val offersDomElements = htmlDocument.select(".offer").not(".promoOffer").asScala
    val offersData = for(offerElement <- offersDomElements)
      yield Offer (
        no = 1,
        name = offerElement.select(".header_title").text(),
        price = offerElement.select(".current-price_value").html().substring(0,offerElement.select(".current-price_value").html().indexOf("&nbsp")).replaceAll("\\s", "").toDouble,
        link = "https://www.itaka.pl"+offerElement.select(".offer_link").attr("href")
      )
    offersData.toList.take(5)
  }

  private def getTraveliadaOffers(dateFrom: DateTime, dateTo: DateTime, countries: List[String], numberOfPersons: Int = 1): List[Offer] = {
    var sourceUrl: String = "https://www.traveliada.pl/wczasy/"
    if (countries != null && countries.nonEmpty) {
      sourceUrl = sourceUrl + "do"
      for(country <- countries) yield sourceUrl = sourceUrl + "," + changePolishSigns(country.toLowerCase)
    }
    if (dateFrom!=null) sourceUrl = sourceUrl + "/t1," + dateFrom.toString("dd-MM-yyyy")
    if (dateTo!=null) sourceUrl = sourceUrl + "/t2," + dateTo.toString("dd-MM-yyyy")
    sourceUrl = sourceUrl + "/adt," + numberOfPersons
    sourceUrl = sourceUrl + "/sort,cena"

    val htmlDocument = Jsoup.connect(sourceUrl).get()
    val recipesDomElements = htmlDocument.select("div.s2o").asScala
    val offersData = for(offerElement <- recipesDomElements) yield Offer(
      no = 1,
      name = offerElement.select(".s2o_hot a").text(),
      price = offerElement.select(".s2o_mob1 .s2o_cena span").text().toDouble,
      link = offerElement.select(".s2o_hot a").attr("href")
    )
    offersData.toList.take(5)
  }

  def getAllCounties: List[String] = {
    val sourceUrl: String = "https://www.itaka.pl/nasze-kierunki/"
    val htmlDocument = Jsoup.connect(s"${sourceUrl}").get()
    val countryDomElements = htmlDocument.select(".fhotel_region_header .fhotel_region_name").asScala
    countryDomElements.map(_.text()).toList
  }

  def getNumbersOfPersons: List[Int] = {
    (1 to 16).toList
  }

  private def changePolishSigns(str: String): String = {
    val original = List("Ą", "ą", "Ć", "ć", "Ę", "ę", "Ł", "ł", "Ń", "ń", "Ó", "ó", "Ś", "ś", "Ź", "ź", "Ż", "ż")
    val normalized = List("A", "a", "C", "c", "E", "e", "L", "l", "N", "n", "O", "o", "S", "s", "Z", "z", "Z", "z")

    val newStr = for (c <- str) yield {
      val index = original.indexOf(c.toString)
      if (index >= 0) normalized(index) else c
    }
    newStr.mkString("")
  }
}
