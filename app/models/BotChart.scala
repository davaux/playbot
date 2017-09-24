package models;

import scala.concurrent.Future
import play.api.libs.ws._
import play.api.libs.json._
import scala.concurrent.duration._

case class ChartData(date: Long, high: Double, low: Double, open: Double, close: Double, volume: Double, quoteVolume: Double, weightedAverage: Double)

class BotChart(exchange: String, pair: String, period: Int) {
	val startTime = 1493640000 // 2017-05-01 12:00:00
	val endTime = 1493726400 // 2017-05-02 12:00:00
	implicit val chartDataReads = Json.reads[ChartData]

	def data(ws: WSClient): Future[JsResult[List[ChartData]]] = {
		import scala.concurrent.ExecutionContext.Implicits.global
		val request: WSRequest = ws.url("https://poloniex.com/public?command=returnChartData")
		val complexRequest: WSRequest =
			request.addHttpHeaders("Accept" -> "application/json")
			.addQueryStringParameters("currencyPair" -> pair)
			.addQueryStringParameters("start" -> startTime.toString)
			.addQueryStringParameters("end" -> endTime.toString)
			.addQueryStringParameters("period" -> period.toString)
			.withRequestTimeout(10000.millis)


		complexRequest.get().map {
			response =>
			//println(s"reponse.json ${(response.json \ pair)}")
			(response.json).validate[List[ChartData]]
		}
	}
}