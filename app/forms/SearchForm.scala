package forms

import org.joda.time.DateTime

case class SearchForm(dateFrom: DateTime, dateTo: DateTime, countries: List[String], numberOfPersons: Int = 1)
