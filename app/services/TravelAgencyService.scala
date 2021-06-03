package services

import javax.inject.Inject
import models.{Offer, ReadOffer}
import org.jsoup.Jsoup
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.{JsValue, Json, Reads, __}
import scalaj.http.{Http, HttpOptions}

import java.text.SimpleDateFormat
import java.util.Date
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


class TravelAgencyService @Inject() (){
  val format1 = new java.text.SimpleDateFormat("yyyy-MM-dd")
  val format2 = new java.text.SimpleDateFormat("dd-MM-yyyy")
  val format3 = new SimpleDateFormat("dd.MM.yyyy")
  var countryCodesTUI: mutable.Map[String, String] = mutable.HashMap("ALBANIA"->"TIA", "ARUBA"->"AUA", "BARBADOS"->"BGI", "BUŁGARIA"->"BG",
    "CHORWACJA"->"HR","CYPR"->"CY", "CZARNOGÓRA"->"TGD", "DJERBA"->"DJE", "DOMINIKANA"->"DO", "EGIPT"->"EG", "GRECJA"->"GR",
    "HISZPANIA"->"ES", "INDONEZJA"->"ID", "JAMAJKA"->"MBJ", "KENIA"->"MBA", "KOSTARYKA"->"SJO", "KUBA"->"CU", "FNC"->"MADERA", "MALEDIWY"->"MLE",
    "MALTA"->"MLA", "MAROKO"->"AGA", "MRU"->"MAURITIUS", "MEKSYK"->"CUN", "PANAMA"->"PTY", "PORTUGALIA"->"PT", "SESZELE"->"SZ",
    "SRI LANKA"->"CMB","PORTUGALIA"->"PT", "STANY ZJEDNOCZONE"->"US", "TAJLANDIA"->"TH", "TUNEZJA"->"TN", "TURCJA"->"TR", "WŁOCHY"->"IT",
    "WYSPY KANARYJSKIE"->"IC", "WYSPY ZIELONEGO PRZYLĄDKA"->"CV", "WŁOCHY"->"IT", "ZANZIBAR"->"ZNZ", "ZJEDNOCZONE EMIRATY ARABSKIE"->"ZEA")

  def getBestOffers(dateFrom: Date, dateTo: Date, countries: List[String], numberOfPersons: Int = 1, minDays: Int = 1, minHotelRate: Int = 3, onlyLastMinute: Boolean = false, onlyAllInclusive: Boolean = false): ListBuffer[Offer] ={
    var bestOffers = new ListBuffer[Offer]()

    bestOffers = ListBuffer.concat(
      getRainbowOffers(dateFrom, dateTo, countries, numberOfPersons, minDays, minHotelRate, onlyLastMinute, onlyAllInclusive),
      getItakaOffers(dateFrom, dateTo, countries, numberOfPersons, minDays, minHotelRate, onlyLastMinute, onlyAllInclusive),
      getTraveliadaOffers(dateFrom, dateTo, countries, numberOfPersons, minDays, minHotelRate, onlyLastMinute, onlyAllInclusive),
      getTUIOffers(dateFrom, dateTo, countries, numberOfPersons, minDays, minHotelRate, onlyLastMinute, onlyAllInclusive))
    bestOffers = bestOffers.sortWith((o1,o2) => o1.price < o2.price)
    bestOffers.take(10)
  }

  def getRainbowOffers(dateFrom: Date, dateTo: Date, countries: List[String], numberOfPersons: Int, minDays: Int, minHotelRate: Int, onlyLastMinute: Boolean, onlyAllInclusive: Boolean): List[Offer] = {
    var body = """{"Konfiguracja":{"LiczbaPokoi":"1","Wiek":["""
    for (_ <- 1 to numberOfPersons) body = body + """"1990-07-14","""
    body = body.dropRight(1)
    body = body + """]},"Sortowanie":{"CzyPoDacie":false,"CzyPoCenie":true,"CzyPoOcenach":false,"CzyPoPolecanych":false,"CzyDesc":false},"CzyCenaZaWszystkich":false,"CzyGrupowac":true,"DlugoscPobytuMin":""""
    body = body + minDays
    body = body + """" ,"KategoriaHoteluMin":""""+ minHotelRate
    body = body + """","MiastaWyjazdu":[], "Panstwa":["""
    if (countries != null && countries.nonEmpty) {
      countries.foreach(c => body = body + """""""  + changeExtraSigns(c.toLowerCase) + """",""")
    }
    body = body.dropRight(1)
    if(onlyLastMinute) body = body + """],"Promocje": ["last-minute""""
    if(onlyAllInclusive) body = body + """],"Wyzywienia": ["all-inclusive""""
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

    if (result.body.isEmpty) return List()
    val json: JsValue = Json.parse(result.body)
    val trips = json \"Bloczki"

    implicit val offerRead: Reads[ReadOffer] = (
        (__  \ "BazoweInformacje" \ "OfertaNazwa").read[String] ~
        ((__ \ "Ceny")(0) \ "CenaZaOsobeAktualna").read[Double] ~
        (__  \ "BazoweInformacje" \ "OfertaURL").read[String] ~
        ((__ \ "Ceny")(0) \ "LiczbaDni").read[Int] ~
        (__ \ "BazoweInformacje" \ "GwiazdkiHotelu").read[Double] ~
        (__ \ "Ocena" \ "Ocena").readWithDefault(0.0) ~
        ((__ \ "Wyzywienia")(0) \ "Nazwa").read[String]
      )(ReadOffer)

    implicit val offersRead: Reads[List[ReadOffer]] = Reads.list(offerRead)
    val offers = trips.get.validate[List[ReadOffer]](offersRead)

    val finalOffers = for (offer <- offers.get) yield Offer (
      no = 0,
      name = offer.name,
      travelAgencyName = "Rainbow",
      price = offer.price,
      link = "https://r.pl" + offer.link,
      duration = offer.duration,
      hotelRate = offer.hotelRate,
      reviewRate = offer.reviewRate,
      isAllInclusive = offer.foodStandard.toLowerCase.contains("all inclusive")
    )
    finalOffers.take(10)
  }

  private def getItakaOffers(dateFrom: Date, dateTo: Date, countries: List[String], numberOfPersons: Int, minDays: Int, minHotelRate: Int, onlyLastMinute: Boolean, onlyAllInclusive: Boolean): List[Offer] = {
    var sourceUrl: String = "https://www.itaka.pl/wyniki-wyszukiwania/wakacje/?view=offerList"+"&adults="+numberOfPersons
    if (dateFrom!=null) sourceUrl = sourceUrl + "&date-from=" + format1.format(dateFrom)
    if (dateTo!=null) sourceUrl = sourceUrl + "&date-to=" + format1.format(dateTo)

    if(onlyAllInclusive) sourceUrl = sourceUrl + "&food=allInclusive"
    if (countries != null && countries.nonEmpty) {
      sourceUrl = sourceUrl + "&dest-region="
      countries.foreach(c => sourceUrl = sourceUrl + changeExtraSigns(c.toLowerCase) + "%2C")
      sourceUrl = sourceUrl.dropRight(3)
    }
    sourceUrl = sourceUrl + "&hotel-rate=" + minHotelRate + "0"
    if (onlyLastMinute) sourceUrl = sourceUrl + "&promo=lastMinute"
    sourceUrl = sourceUrl + "&order=priceAsc"
    sourceUrl = sourceUrl + "&transport=flight"

    minDays match {
      case 1 | 2 | 3 | 4 | 5 => sourceUrl = sourceUrl + "&duration=to6"
      case 6 | 7 | 8 => sourceUrl = sourceUrl + "&duration=from6to9"
      case 9 | 10 | 12 => sourceUrl = sourceUrl + "&duration=from9to12"
    }
    if(minDays > 12) sourceUrl = sourceUrl + "&duration=from13"

    val htmlDocument = Jsoup.connect(sourceUrl).get()
    if (!htmlDocument.select(".empty-results").isEmpty) return List()
    val offersDomElements = htmlDocument.select(".offer").not(".promoOffer").asScala
    val offersData = for(offerElement <- offersDomElements)
      yield Offer (
        no = 1,
        name = offerElement.select(".header_title").text(),
        travelAgencyName = "Itaka",
        price = offerElement.select(".current-price_value").html().substring(0,offerElement.select(".current-price_value").html().indexOf("&nbsp")).replaceAll("\\s", "").toDouble,
        link = "https://www.itaka.pl"+offerElement.select(".offer_link").attr("href"),
        duration = offerElement.select(".offer_date span").not(".offer_date_icon-container").text().substring(16, 17).toInt,
        hotelRate = offerElement.select(".star").toArray().length - offerElement.select(".star_half").toArray().length * 0.5,
        reviewRate = replaceNullStringToZeroString(offerElement.select(".hotel-rank").text()).toDouble,
        isAllInclusive = offerElement.select(".offer_food").text() == "All inclusive"
      )

    offersData.toList.filter(offer => offer.duration >= minDays).take(10)
  }

  private def getTraveliadaOffers(dateFrom: Date, dateTo: Date, countries: List[String], numberOfPersons: Int, minDays: Int, minHotelRate: Int, onlyLastMinute: Boolean, onlyAllInclusive: Boolean): List[Offer] = {
    var sourceUrl: String = "https://www.traveliada.pl/wczasy/"
    if (countries != null && countries.nonEmpty) {
      sourceUrl = sourceUrl + "do"
      countries.foreach(c => sourceUrl = sourceUrl + "," + changeExtraSigns(c.toLowerCase))
    }
    if (onlyLastMinute) sourceUrl = sourceUrl + "/last-minute"
    if (onlyAllInclusive) sourceUrl = sourceUrl + "/all-inclusive"

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
      travelAgencyName = "Traveliada",
      price = offerElement.select(".s2o_mob1 .s2o_cena span").text().toDouble,
      link = offerElement.select(".s2o_hot a").attr("href"),
      duration = offerElement.select(".s2o_mob1 .s2o_dni").textNodes().get(0).text().substring(0,1).toInt,
      hotelRate = offerElement.select(""".s2o_star img[src="/themes/images/star_1.png"]""").size(),
      reviewRate = BigDecimal(replaceNullStringToZeroString(offerElement.select(".s2o_ocena").text()).substring(0,3).toDouble*6/10).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble,
      isAllInclusive = offerElement.select(".s2o_w").text().contains("all inclusive")
    )
    offersData.toList.filter(offer => offer.duration >= minDays).take(10)
  }

  private def getTUIOffers(dateFrom: Date, dateTo: Date, countries: List[String], numberOfPersons: Int, minDays: Int, minHotelRate: Int,onlyLastMinute: Boolean, onlyAllInclusive: Boolean): List[Offer] = {
    val country = countries.head
    var countryShortcut = ""
    if(!countryCodesTUI.contains(country.toUpperCase())) {
      return null
    } else {
      countryShortcut = countryCodesTUI.get(country.toUpperCase()).get
    }
    val dateFromFormated = format3.format(dateFrom)
    val dateToFormatted = format3.format(dateTo)
    var body = """{"offerType":"BY_PLANE","childrenBirthdays":[],"departureDateFrom":""""+dateFromFormated+"""","departureDateTo":""""+dateToFormatted+"""","departuresCodes":[],"destinationsCodes":["""" + countryShortcut
    body = body + """"],"numberOfAdults":""" + numberOfPersons+""","durationFrom":""""+minDays+"""","durationTo":"30","site":"wypoczynek/wyniki-wyszukiwania-samolot","metaData":{"page":0,"pageSize":30,"sorting":"price"},"filters":[{"filterId":"priceSelector","selectedValues":[]},{"filterId":"board","selectedValues":[]},{"filterId":"amountRange","selectedValues":[""]},
                 {"filterId":"flight_category","selectedValues":[]},{"filterId":"minHotelCategory","selectedValues":[""""
    body = body + minHotelRate+"""s"]},{"filterId":"tripAdvisorRating","selectedValues":["defaultTripAdvisorRating"]},{"filterId":"beach_distance","selectedValues":["defaultBeachDistance"]},{"filterId":"facilities","selectedValues":[]},{"filterId":"WIFI","selectedValues":[]},
                              {"filterId":"sport_and_wellness","selectedValues":[]},{"filterId":"room_type","selectedValues":[]},{"filterId":"additionalType","selectedValues":[]}]}"""
    val result = Http("https://www.tui.pl/search/offers")
      .postData(body)
      .header("Content-Type", "application/json")
      .header("Charset", "UTF-8")
      .option(HttpOptions.readTimeout(1000000)).asString
    val json: JsValue = Json.parse(result.body)
    val trips = json \ "offers"

    implicit val offerRead: Reads[ReadOffer] = (
        (__  \ "hotelName").read[String] ~
        (__ \ "discountPerPersonPrice").read[String].map(f => f.toDouble) ~
        (__  \ "offerUrl").read[String] ~
        (__ \ "duration").read[Int] ~
        (__ \ "hotelStandard").read[Double] ~
        (__ \ "tripAdvisorRating").readWithDefault(0.0)~
        (__ \ "boardType").read[String]
      )(ReadOffer)
    implicit val offersRead: Reads[List[ReadOffer]] = Reads.list(offerRead)
    val offers = trips.get.validate[List[ReadOffer]](offersRead)
    println("sas")


    val finalOffers = for (offer <- offers.get) yield Offer (
      no = 0,
      name = offer.name,
      travelAgencyName = "TUI",
      price = offer.price,
      link = "https://www.tui.pl" + offer.link,
      duration = offer.duration,
      hotelRate = offer.hotelRate,
      reviewRate = offer.reviewRate,
      isAllInclusive = offer.foodStandard.toLowerCase.contains("all inclusive")
    )
    finalOffers.take(10)
  }

  private def replaceNullStringToZeroString(string: String): String = {
    if (string == null || string.isEmpty) "0000" else string
  }

  def getAllCounties: List[String] = {
    val sourceUrl: String = "https://www.itaka.pl/nasze-kierunki/"
    val htmlDocument = Jsoup.connect(s"${sourceUrl}").get()
    val countryDomElements = htmlDocument.select(".fhotel_region_header .fhotel_region_name").asScala
    countryDomElements.map(_.text()).toList.filter(c => !c.contains("(narty)"))
  }

  private def changeExtraSigns(str: String): String = {
    val original = List("Ą", "ą", "Ć", "ć", "Ę", "ę", "Ł", "ł", "Ń", "ń", "Ó", "ó", "Ś", "ś", "Ź", "ź", "Ż", "ż", "ç", " ")
    val normalized = List("A", "a", "C", "c", "E", "e", "L", "l", "N", "n", "O", "o", "S", "s", "Z", "z", "Z", "z", "c", "-")

    val newStr = for (c <- str) yield {
      val index = original.indexOf(c.toString)
      if (index >= 0) normalized(index) else c
    }
    newStr.mkString("")
  }
}
