package models;

import play.api.Logger
import scala.concurrent.Future
import play.api.libs.ws._
import play.api.libs.json._
import scala.concurrent.duration._
import java.text.SimpleDateFormat
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import java.text.SimpleDateFormat
import java.util.{Calendar, Date}
import scala.util.{Failure, Success}
import play.api.Configuration

case class ChartData(date: Long, high: Double, low: Double, open: Double, close: Double, volume: Double, quoteVolume: Double, weightedAverage: Double)

case class ChartDataBittrex(O: Double, H: Double, L: Double, C: Double, V: Double, T: String, BV: Double)

class BotChart(config: Configuration, ws: WSClient, exchange: String, pair: String, period: Int, backtest: Boolean = true) {
	//val startTime = 1493640000 // 2017-05-01 12:00:00
	//val endTime = 1493726400 // 2017-05-02 12:00:00
	val dateFormat = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
    val parsedStartDate = dateFormat.parse("2017-10-13 12:00:00");
    val parsedEndDate = dateFormat.parse("2017-10-14 12:00:00");
    val calStartDate = Calendar.getInstance
    calStartDate.setTime(parsedStartDate)
    val calEndDate = Calendar.getInstance
    calEndDate.setTime(parsedEndDate)
	val startTime = calStartDate.getTimeInMillis / 1000//1504267200
	val endTime = calEndDate.getTimeInMillis / 1000//1506600000
	implicit val chartDataReads = Json.reads[ChartData]
	implicit val chartDataBittrexReads = Json.reads[ChartDataBittrex]
	val format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")

	def data(): Future[List[ChartData]] = {
		if(exchange == "poloniex") {
			val futureResult: Future[JsResult[List[ChartData]]] = dataPoloniex()
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
			val futureResult: Future[JsResult[List[ChartDataBittrex]]] = dataBittrex()
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

	private def dataPoloniex(): Future[JsResult[List[ChartData]]] = {
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
	private def dataBittrex(): Future[JsResult[List[ChartDataBittrex]]] = {
		val request: WSRequest = ws.url("https://bittrex.com/Api/v2.0/pub/market/GetTicks")
		val complexRequest: WSRequest =
			request.addHttpHeaders("Accept" -> "application/json")
			.addQueryStringParameters("marketName" -> pair.replace("_", "-"))
			.addQueryStringParameters("tickInterval" -> "fiveMin")
			/*.addQueryStringParameters("_" -> "1506600000")*/
			.withRequestTimeout(10000.millis)


		complexRequest.get().map {
			response =>
				//Logger.info(s"response: ${(response.json \ "result")}")
				//println(s"reponse.json ${(response.json \ pair)}")
				(response.json \ "result").validate[List[ChartDataBittrex]]
		}
	}

	def getCurrentPrice(): Future[Option[Double]] = {
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
					val result: JsResult[Double] = (response.json \ "result" \ "Last").validate[Double]
					result match {
						case s: JsSuccess[Double] => {
							Some(s.get)
						}
						case e: JsError => {
							Logger.error(JsError.toJson(e).toString())
							None
						}
					}
			}
		}
	}

	def getBalance(currency: String = "BTC"): Future[Option[Double]] = {
		//if(exchange == "bittrex") {

			val apiKey = config.get[String]("api.key")
			val nonce = Calendar.getInstance.getTimeInMillis / 1000
			val uriBase = "https://bittrex.com/api/v1.1/account/getbalance"
			val uri = s"${uriBase}?apikey=${apiKey}&nonce=${nonce}&currency=${currency}"
			val hash = sign(uri)
			val request: WSRequest = ws.url(uriBase)
			val complexRequest: WSRequest =
			request.addHttpHeaders("Accept" -> "application/json", "apisign" -> hash)
				.addQueryStringParameters("apikey" -> apiKey)
				.addQueryStringParameters("nonce" -> nonce.toString)
				.addQueryStringParameters("currency" -> currency)
				.withRequestTimeout(10000.millis)
			complexRequest.get().map {
				response =>
					Logger.info("reponse " + response.json)
					val result: JsResult[Double] = (response.json \ "result" \ "Available").validate[Double]
					result match {
						case s: JsSuccess[Double] => {
							Some(s.get)
						}
						case e: JsError => {
							Logger.error(JsError.toJson(e).toString())
							None
						}
					}
			}
		//}
	}

	def bittrexBuy(quantity: Double, rate: Double): Future[Option[String]] = {
		//"https://bittrex.com/api/v1.1/market/buylimit?apikey=API_KEY&market=BTC-LTC&quantity=1.2&rate=1.3"
		val market = pair.replace("_", "-")
		val sRate = "%.8f" format rate
		val sQuantity = "%.8f" format quantity
		val apiKey = config.get[String]("api.key")
		val nonce = Calendar.getInstance.getTimeInMillis / 1000
		val uriBase = "https://bittrex.com/api/v1.1/market/buylimit"
		val uri = s"${uriBase}?apikey=${apiKey}&nonce=${nonce}&market=${market}&quantity=${sQuantity}&rate=${sRate}"
		Logger.info(s"uri: $uri")
		val hash = sign(uri)
		val request: WSRequest = ws.url(uriBase)
		val complexRequest: WSRequest =
		request.addHttpHeaders("Accept" -> "application/json", "apisign" -> hash)
			.addQueryStringParameters("apikey" -> apiKey)
			.addQueryStringParameters("nonce" -> nonce.toString)
			.addQueryStringParameters("market" -> market)
			.addQueryStringParameters("quantity" -> sQuantity)
			.addQueryStringParameters("rate" -> sRate)
			.withRequestTimeout(10000.millis)
		complexRequest.get().map {
			response =>
				Logger.info("reponse " + response.json)
				val result: JsResult[String] = (response.json \ "result" \ "uuid").validate[String]
				result match {
					case s: JsSuccess[String] => {
						Some(s.get)
					}
					case e: JsError => {
						Logger.error(JsError.toJson(e).toString())
						None
					}
				}
		}
	}


	def bittrexSell(quantity: Double, rate: Double): Future[Option[String]] = {
		//"https://bittrex.com/api/v1.1/market/selllimit?apikey=API_KEY&market=BTC-LTC&quantity=1.2&rate=1.3"
		val market = pair.replace("_", "-")
		val sRate = "%.8f" format rate
		val sQuantity = "%.8f" format quantity
		val apiKey = config.get[String]("api.key")
		val nonce = Calendar.getInstance.getTimeInMillis / 1000
		val uriBase = "https://bittrex.com/api/v1.1/market/selllimit"
		val uri = s"${uriBase}?apikey=${apiKey}&nonce=${nonce}&market=${market}&quantity=${sQuantity}&rate=${sRate}"
		val hash = sign(uri)
		val request: WSRequest = ws.url(uriBase)
		val complexRequest: WSRequest =
		request.addHttpHeaders("Accept" -> "application/json", "apisign" -> hash)
			.addQueryStringParameters("apikey" -> apiKey)
			.addQueryStringParameters("nonce" -> nonce.toString)
			.addQueryStringParameters("market" -> market)
			.addQueryStringParameters("quantity" -> sQuantity)
			.addQueryStringParameters("rate" -> sRate)
			.withRequestTimeout(10000.millis)
		complexRequest.get().map {
			response =>
				Logger.info("reponse " + response.json)
				val result: JsResult[String] = (response.json \ "result" \ "uuid").validate[String]
				result match {
					case s: JsSuccess[String] => {
						Some(s.get)
					}
					case e: JsError => {
						Logger.error(JsError.toJson(e).toString())
						None
					}
				}
		}
	}

	def sell(): Unit = {
		val apiKey = config.get[String]("api.key")
		val nonce = Calendar.getInstance.getTimeInMillis / 1000
		val uriBase = "https://bittrex.com/api/v1.1/market/selllimit"
		val uri = s"${uriBase}?apikey=${apiKey}&nonce=${nonce}&market=${pair.replace("_", "-")}&quantity=1&rate=1"
		val hash = sign(uri)
		val request: WSRequest = ws.url(uriBase)
		val complexRequest: WSRequest =
		request.addHttpHeaders("Accept" -> "application/json", "apisign" -> hash)
			.addQueryStringParameters("apikey" -> apiKey)
			.addQueryStringParameters("nonce" -> nonce.toString)
			.addQueryStringParameters("market" -> pair.replace("_", "-"))
			.addQueryStringParameters("quantity" -> "1")
			.addQueryStringParameters("rate" -> "1")
			.withRequestTimeout(10000.millis)
		complexRequest.get().map {
			response =>
				Logger.info("reponse " + response.json)
		}
	}

	def bittrexOpenOrders(): Unit = {
		val apiKey = config.get[String]("api.key")
		val nonce = Calendar.getInstance.getTimeInMillis / 1000
		val uriBase = "https://bittrex.com/api/v1.1/market/getopenorders"
		val uri = s"${uriBase}?apikey=${apiKey}&nonce=${nonce}&market=${pair.replace("_", "-")}"
		val hash = sign(uri)
		val request: WSRequest = ws.url(uriBase)
		val complexRequest: WSRequest =
		request.addHttpHeaders("Accept" -> "application/json", "apisign" -> hash)
			.addQueryStringParameters("apikey" -> apiKey)
			.addQueryStringParameters("nonce" -> nonce.toString)
			.addQueryStringParameters("market" -> pair.replace("_", "-"))
			.withRequestTimeout(10000.millis)
		complexRequest.get().map {
			response =>
				Logger.info("reponse " + response.json)
		}

	}

	private def sign(uri: String) = {
		import com.roundeights.hasher.{Hasher, Algo}
		import scala.language.postfixOps

		val apiSecret = config.get[String]("api.secret")
		val algo = Algo.hmac(apiSecret).sha512
		val hash = algo(uri)
		hash.hex
	}
}