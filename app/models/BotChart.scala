package models;

import play.api.Logger
import scala.concurrent.Future
import play.api.libs.ws._
import play.api.libs.json._
import scala.concurrent.duration._
import java.text.SimpleDateFormat
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global

case class ChartData(date: Long, high: Double, low: Double, open: Double, close: Double, volume: Double, quoteVolume: Double, weightedAverage: Double)

case class ChartDataBittrex(O: Double, H: Double, L: Double, C: Double, V: Double, T: String, BV: Double)

class BotChart(exchange: String, pair: String, period: Int, backtest: Boolean = true) {
	//val startTime = 1493640000 // 2017-05-01 12:00:00
	//val endTime = 1493726400 // 2017-05-02 12:00:00
	val startTime = 1506600000
	val endTime = 1506686400
	implicit val chartDataReads = Json.reads[ChartData]
	implicit val chartDataBittrexReads = Json.reads[ChartDataBittrex]
	val format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")

	def data(ws: WSClient): Future[List[ChartData]] = {
		if(exchange == "poloniex") {
			val futureResult: Future[JsResult[List[ChartData]]] = dataPoloniex(ws)
			futureResult.map {
				chartDataResult =>
					chartDataResult match {
						case s: JsSuccess[List[ChartData]] => s.get
						case e: JsError => {
							Logger.error(JsError.toJson(e).toString())
							List.empty[ChartData]
						}
					}
			}
		} else if(exchange == "bittrex") {
			val futureResult: Future[JsResult[List[ChartDataBittrex]]] = dataBittrex(ws)
			var chartData: ListBuffer[ChartData] = new ListBuffer[ChartData]()
			futureResult.map {
				chartDataResult =>
					chartDataResult match {
						case s: JsSuccess[List[ChartDataBittrex]] => {
							for(bittrexData <- s.get) {
								val date = format.parse(bittrexData.T)
								chartData += ChartData(date.getTime() / 1000, bittrexData.H, bittrexData.L, bittrexData.O, bittrexData.C, 0.0, 0.0, bittrexData.C)
							}
							chartData.toList
						}
						case e: JsError => {
							Logger.error(JsError.toJson(e).toString())
							List.empty[ChartData]
						}
					}
			}
		} else {
			Future {
				List.empty[ChartData]
			}
		}
	}

	def dataPoloniex(ws: WSClient): Future[JsResult[List[ChartData]]] = {
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

	// https://bittrex.com/Api/v2.0/pub/market/GetTicks?marketName=BTC-STEEM&tickInterval=thirtyMin&_=1493640000
	def dataBittrex(ws: WSClient): Future[JsResult[List[ChartDataBittrex]]] = {
		val request: WSRequest = ws.url("https://bittrex.com/Api/v2.0/pub/market/GetTicks")
		val complexRequest: WSRequest =
			request.addHttpHeaders("Accept" -> "application/json")
			.addQueryStringParameters("marketName" -> pair.replace("_", "-"))
			.addQueryStringParameters("tickInterval" -> "thirtyMin")
			/*.addQueryStringParameters("_" -> "1506600000")*/
			.withRequestTimeout(10000.millis)


		complexRequest.get().map {
			response =>
				Logger.info(s"response: ${(response.json \ "result")}")
				//println(s"reponse.json ${(response.json \ pair)}")
				(response.json \ "result").validate[List[ChartDataBittrex]]
		}
	}

	def getCurrentPrice(ws: WSClient): Future[Option[Double]] = {
		if(exchange == "poloniex") {
			val request: WSRequest = ws.url("https://poloniex.com/public?command=returnTicker")
			val complexRequest: WSRequest =
			request.addHttpHeaders("Accept" -> "application/json")
			  /*.addQueryStringParameters("market" -> pair)*/
			  .withRequestTimeout(10000.millis)
			complexRequest.get().map {
				response =>
					//println(s"reponse.json ${(response.json \ pair \ "last")}")
					val result: JsResult[String] = (response.json \ pair \ "last").validate[String]
					result match {
						case s: JsSuccess[String] => Some(s.get.toDouble)
						case e: JsError => {
							Logger.error(JsError.toJson(e).toString())
							None
						}
					}
			}
		} else {
			val request: WSRequest = ws.url("https://bittrex.com/api/v1.1/public/getticker")
			val complexRequest: WSRequest =
			request.addHttpHeaders("Accept" -> "application/json")
			  .addQueryStringParameters("market" -> pair.replace("_", "-"))
			  .withRequestTimeout(10000.millis)
			complexRequest.get().map {
				response =>
					//println(s"reponse.json ${(response.json \ pair \ "last")}")
					val result: JsResult[String] = (response.json \ "result" \ "Last").validate[String]
					result match {
						case s: JsSuccess[String] => Some(s.get.toDouble)
						case e: JsError => {
							Logger.error(JsError.toJson(e).toString())
							None
						}
					}
			}
		}
	}
}