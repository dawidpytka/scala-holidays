package services

import javax.inject.Inject
import models.Offer
import org.jsoup.Jsoup
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.{JsValue, Json, Reads, __}
import scalaj.http.{Http, HttpOptions}

import java.util.Date
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer


class TravelAgencyService @Inject() (){
  val format1 = new java.text.SimpleDateFormat("yyyy-MM-dd")
  val format2 = new java.text.SimpleDateFormat("dd-MM-yyyy")


  def getBestOffers(dateFrom: Date, dateTo: Date, countries: List[String], numberOfPersons: Int = 1, minDays: Int = 1, minHotelRate: Int = 3): ListBuffer[Offer] ={
    var bestOffers = new ListBuffer[Offer]()

    bestOffers = ListBuffer.concat(
      getRainbowOffers(dateFrom, dateTo, countries, numberOfPersons, minDays, minHotelRate),
      getItakaOffers(dateFrom, dateTo, countries, numberOfPersons, minDays, minHotelRate),
      getTraveliadaOffers(dateFrom, dateTo, countries, numberOfPersons, minDays, minHotelRate))

    bestOffers = bestOffers.sortWith((o1,o2) => o1.price < o2.price)
    bestOffers.take(5)
  }

  def getRainbowOffers(dateFrom: Date, dateTo: Date, countries: List[String], numberOfPersons: Int, minDays: Int, minHotelRate: Int): List[Offer] = {
    var body = """{"Konfiguracja":{"LiczbaPokoi":"1","Wiek":["""
    for (_ <- 1 to numberOfPersons) body = body + """"1990-07-14","""
    body = body.dropRight(1)
    body = body + """]},"Sortowanie":{"CzyPoDacie":false,"CzyPoCenie":true,"CzyPoOcenach":false,"CzyPoPolecanych":false,"CzyDesc":false},"CzyCenaZaWszystkich":false,"CzyGrupowac":true,"DlugoscPobytuMin":""""
    body = body + minDays
    body = body + """" ,"KategoriaHoteluMin":""""+ minHotelRate
    body = body + """","MiastaWyjazdu":[], "Panstwa":["""
    if (countries != null && countries.nonEmpty) {
      countries.foreach(c => body = body + """""""  + changePolishSigns(c.toLowerCase) + """",""")
    }
    body = body.dropRight(1)
    body = body + """],"TerminWyjazduMin":""""
    if (dateFrom!=null) body = body + format1.format(dateFrom)
    body = body + """","TerminWyjazduMax":""""
    if (dateTo!=null) body = body + format1.format(dateTo)

    body = body + """","TypyTransportu":["air"],"CzyPotwierdzoneTerminy":false,"PokazywaneLotniska":"SAME","Paginacja":{"Przeczytane":"0","IloscDoPobrania":"18"},"CzyImprezaWeekendowa":false}"""

    val result = Http("https://rpl-api.r.pl/v3/wyszukiwarka/api/wyszukaj")
      .postData(body)
      .header("Content-Type", "application/json")
      .header("Charset", "UTF-8")
      .option(HttpOptions.readTimeout(1000000)).asString

    val json: JsValue = Json.parse(result.body)
    val trips = json \"Bloczki"

    implicit val offerRead: Reads[Offer] = (
      (__  \ "BazoweInformacje" \ "HotelID" ).read[Int] ~
        (__  \ "BazoweInformacje" \ "OfertaNazwa").read[String] ~
        ((__ \ "Ceny")(0) \ "CenaZaOsobeAktualna").read[Double] ~
        (__  \ "BazoweInformacje" \ "OfertaURL").read[String] ~
        ((__ \ "Ceny")(0) \ "LiczbaDni").read[Int] ~
        (__ \ "BazoweInformacje" \ "GwiazdkiHotelu").read[Double]
      )(Offer)

    implicit val offersRead: Reads[List[Offer]] = Reads.list(offerRead)
    val offers = trips.get.validate[List[Offer]](offersRead)

    val finalOffers = for (offer <- offers.get) yield Offer (
      no = offer.no,
      name = offer.name,
      price = offer.price,
      link = "https://r.pl" + offer.link,
      duration = offer.duration,
      hotelRate = offer.hotelRate
    )
    finalOffers.take(5)
  }

  private def getItakaOffers(dateFrom: Date, dateTo: Date, countries: List[String], numberOfPersons: Int, minDays: Int, minHotelRate: Int): List[Offer] = {
    var sourceUrl: String = "https://www.itaka.pl/wyniki-wyszukiwania/wakacje/?view=offerList"+"&adults="+numberOfPersons
    if (dateFrom!=null) sourceUrl = sourceUrl + "&date-from=" + format1.format(dateFrom)
    if (dateTo!=null) sourceUrl = sourceUrl + "&date-to=" + format1.format(dateTo)

    if (countries != null && countries.nonEmpty) {
      sourceUrl = sourceUrl + "&dest-region="
      countries.foreach(c => sourceUrl = sourceUrl + changePolishSigns(c.toLowerCase) + "%2C")
      sourceUrl = sourceUrl.dropRight(3)
    }
    sourceUrl = sourceUrl + "&hotel-rate=" + minHotelRate + "0"
    sourceUrl = sourceUrl + "&review-rate=" + minHotelRate + "0"
    sourceUrl = sourceUrl + "&order=priceAsc"
    sourceUrl = sourceUrl + "&transport=flight"

    minDays match {
      case 1 | 2 | 3 | 4 | 5 => sourceUrl = sourceUrl + "&duration=to6"
      case 6 | 7 | 8 => sourceUrl = sourceUrl + "&duration=from6to9"
      case 9 | 10 | 12 => sourceUrl = sourceUrl + "&duration=from9to12"
    }
    if(minDays > 12) sourceUrl = sourceUrl + "&duration=from13"

    val htmlDocument = Jsoup.connect(sourceUrl).get()
    val offersDomElements = htmlDocument.select(".offer").not(".promoOffer").asScala
    val offersData = for(offerElement <- offersDomElements)
      yield Offer (
        no = 1,
        name = offerElement.select(".header_title").text(),
        price = offerElement.select(".current-price_value").html().substring(0,offerElement.select(".current-price_value").html().indexOf("&nbsp")).replaceAll("\\s", "").toDouble,
        link = "https://www.itaka.pl"+offerElement.select(".offer_link").attr("href"),
        duration = offerElement.select(".offer_date span").not(".offer_date_icon-container").text().substring(16, 17).toInt,
        hotelRate = offerElement.select(".star").toArray().length - offerElement.select(".star_half").toArray().length * 0.5
      )

    offersData.toList.filter(offer => offer.duration >= minDays).take(5)
  }

  private def getTraveliadaOffers(dateFrom: Date, dateTo: Date, countries: List[String], numberOfPersons: Int, minDays: Int, minHotelRate: Int): List[Offer] = {
    var sourceUrl: String = "https://www.traveliada.pl/wczasy/"
    if (countries != null && countries.nonEmpty) {
      sourceUrl = sourceUrl + "do"
      countries.foreach(c => sourceUrl = sourceUrl + "," + changePolishSigns(c.toLowerCase))
    }

    if (dateFrom!=null) sourceUrl = sourceUrl + "/t1," + format2.format(dateFrom)
    if (dateTo!=null) sourceUrl = sourceUrl + "/t2," + format2.format(dateTo)
    sourceUrl = sourceUrl + "/adt," + numberOfPersons
    sourceUrl = sourceUrl + "/samolotem"
    sourceUrl = sourceUrl + "/s," + minHotelRate + "-5"
    sourceUrl = sourceUrl + "/sort,cena"
    sourceUrl = sourceUrl + "/dni," + minDays + "-21"

    val htmlDocument = Jsoup.connect(sourceUrl).get()
    val offersDomElements = htmlDocument.select("div.s2o").asScala
    val offersData = for(offerElement <- offersDomElements) yield Offer(
      no = 1,
      name = offerElement.select(".s2o_hot a").text(),
      price = offerElement.select(".s2o_mob1 .s2o_cena span").text().toDouble,
      link = offerElement.select(".s2o_hot a").attr("href"),
      duration = offerElement.select(".s2o_mob1 .s2o_dni").textNodes().get(0).text().substring(0,1).toInt,
      hotelRate = offerElement.select(""".s2o_star img[src="/themes/images/star_1.png"]""").size()
    )
    offersData.toList.filter(offer => offer.duration >= minDays).take(5)
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
