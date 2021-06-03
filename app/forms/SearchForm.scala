package forms


import java.util.Date

case class SearchForm(dateFrom: Date, dateTo: Date, minDaysAmount : Int, starsAmount : Int, personsAmount : Int, Country: String, isAllInclusive : Boolean,
                      isLastMinute : Boolean)
