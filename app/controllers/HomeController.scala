package controllers

import play.api.Logger
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.collection.mutable.ListBuffer

import scala.util.{Failure, Success}
import akka.actor.{ActorRef, ActorSystem}

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.libs.ws._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import java.text.SimpleDateFormat
import java.util.Calendar
import models._

case class Ticker(id: Int, last: Double, lowestAsk: Double, highestBid: Double, 
  percentChange: Double, baseVolume: Double, quoteVolume: Double, isFrozen: Int, 
  high24hr: Double, low24hr: Double)

case class DataPoint(date: String, price: String, trend: String, label: String, desc: String)

object DataPoint {
  implicit val dataPointWrites = Json.writes[DataPoint]
}

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(cc: ControllerComponents, ws: WSClient, actorSystem: ActorSystem) extends AbstractController(cc) {

  implicit val tickerReads: Reads[Ticker] = (
    (JsPath \ "id").read[Int] and
    (JsPath \ "last").read[String].map[Double](_.toDouble) and
    (JsPath \ "lowestAsk").read[String].map[Double](_.toDouble) and
    (JsPath \ "highestBid").read[String].map[Double](_.toDouble) and
    (JsPath \ "percentChange").read[String].map[Double](_.toDouble) and
    (JsPath \ "baseVolume").read[String].map[Double](_.toDouble) and
    (JsPath \ "quoteVolume").read[String].map[Double](_.toDouble) and
    (JsPath \ "isFrozen").read[String].map[Int](_.toInt) and
    (JsPath \ "high24hr").read[String].map[Double](_.toDouble) and
    (JsPath \ "low24hr").read[String].map[Double](_.toDouble)
  )(Ticker.apply _)
  //implicit val chartDataReads = Json.reads[ChartData]
  val periods = Array(300, 900, 1800, 7200, 14400, 86400)
  val format = new SimpleDateFormat("yyy-MM-dd HH:mm:ss")
  val pair = "BTC_ETH"
  //val period = periods(0).seconds
  var prices: ListBuffer[Double] = new ListBuffer[Double]()
  var currentMovingAverage: Double = 0;
  val lengthOfMA = 10;
  var previousPrice: Double = 0
  var tradePlaced = false
  var typeOfTrade = ""
  var dataPoints: ListBuffer[DataPoint] = new ListBuffer[DataPoint]()
  val botChart = new BotChart("poloniex", pair, periods(3))
  val strategy = new BotStrategy();

  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index() = Action.async { implicit request: Request[AnyContent] =>
    import scala.concurrent.ExecutionContext.Implicits.global
    /*if(startTime.isDefined && endTime.isDefined) {*/
      val historicalData = botChart.data(ws);

      historicalData.map(value => 
        {
          for(candleStick <- value.get) {
            val lastPairPrice = candleStick.weightedAverage
            val dataDate = format.format(candleStick.date * 1000L)
            strategy.tick(candleStick)
            /*Logger.info("" + candleStick)*/

            /*if(prices.length > 0) {
              currentMovingAverage = prices.sum / prices.length
              previousPrice = prices(prices.size - 1)
              if(!tradePlaced) {
                if((lastPairPrice > currentMovingAverage) && (lastPairPrice < previousPrice)) {
                  println("SELL ORDER")
                  tradePlaced = true;
                  typeOfTrade = "short"
                } else if((lastPairPrice < currentMovingAverage) && (lastPairPrice > previousPrice)) {
                  println("BUY ORDER")
                  tradePlaced = true;
                  typeOfTrade = "long"
                }
              } else if (typeOfTrade == "short") {
                if(lastPairPrice < currentMovingAverage) {
                  println("EXIT TRADE")
                  tradePlaced = false
                  typeOfTrade = ""
                }
              } else if (typeOfTrade == "long") {
                if(lastPairPrice > currentMovingAverage) {
                  println("EXIT TRADE")
                  tradePlaced = false
                  typeOfTrade = ""
                }
              }

            } else {
              previousPrice = 0
            }*/
            dataPoints += new DataPoint(dataDate, lastPairPrice.toString, "", "", "")
            //println(s"$dataDate period...$period, $pair, $lastPairPrice, Moving Average $currentMovingAverage")

            /*prices += lastPairPrice
            prices = prices.slice(prices.size - lengthOfMA, prices.size)*/
          }
          val xLabels = dataPoints.map(_.date);
          val xSeries = for (i <- List.range(1, xLabels.size + 1)) yield i.toDouble
          //println(xSeries)
          val ySeries = dataPoints.map(_.price.toDouble).toList
          //println(ySeries)
          val leastSquaresCoeff = leastSquares(xSeries, ySeries);
          //println(leastSquaresCoeff(0))
          //println(leastSquaresCoeff(1))
          //println(leastSquaresCoeff(2))
          Logger.info(s"Total Gain : ${strategy.totalGain}")
          Logger.info(s"Total Loss : ${strategy.totalLoss}")
          Ok(views.html.index(dataPoints.toList))
        }

      )

      /*historicalData.onComplete {
        case Success(value) => {
          for(candleStick <- value.get) {
            val lastPairPrice = candleStick.weightedAverage
            val dataDate = format.format(candleStick.date * 1000L)

            if(prices.length > 0) {
              currentMovingAverage = prices.sum / prices.length
              previousPrice = prices(prices.size - 1)
              if(!tradePlaced) {
                if((lastPairPrice > currentMovingAverage) && (lastPairPrice < previousPrice)) {
                  println("SELL ORDER")
                  tradePlaced = true;
                  typeOfTrade = "short"
                } else if((lastPairPrice < currentMovingAverage) && (lastPairPrice > previousPrice)) {
                  println("BUY ORDER")
                  tradePlaced = true;
                  typeOfTrade = "long"
                }
              } else if (typeOfTrade == "short") {
                if(lastPairPrice < currentMovingAverage) {
                  println("EXIT TRADE")
                  tradePlaced = false
                  typeOfTrade = ""
                }
              } else if (typeOfTrade == "long") {
                if(lastPairPrice > currentMovingAverage) {
                  println("EXIT TRADE")
                  tradePlaced = false
                  typeOfTrade = ""
                }
              }

            } else {
              previousPrice = 0
            }
            dataPoints += new DataPoint(dataDate, lastPairPrice.toString, "", "", "")
            println(s"$dataDate period...$period, $pair, $lastPairPrice, Moving Average $currentMovingAverage")

            prices += lastPairPrice
            prices = prices.slice(prices.size - lengthOfMA, prices.size)
          }
        }
        case Failure(e) => e.printStackTrace


        Ok(views.html.index(Json.toJson(dataPoints.toList)))
      }*/
    /*} else {
      val request: WSRequest = ws.url("https://poloniex.com/public?command=returnTicker")
      val complexRequest: WSRequest =
        request.addHttpHeaders("Accept" -> "application/json")
          /*.addQueryStringParameters("market" -> "BTC-STEEM")*/
          .withRequestTimeout(10000.millis)


      actorSystem.scheduler.schedule(initialDelay = 10.seconds, interval = period) {
        val futureResult: Future[JsResult[Ticker]] = complexRequest.get().map {
          response =>
            //println(s"reponse.json ${(response.json \ pair)}")
            (response.json \ pair).validate[Ticker]
        }
        
        futureResult.onComplete {
          case Success(value) => {
            val lastPairPrice = value.get.last

            if(prices.length > 0) {
              currentMovingAverage = prices.sum / prices.length
            }
            println(format.format(Calendar.getInstance().getTime()) + s" period...$period, $pair, $lastPairPrice, Moving Average $currentMovingAverage")

            prices += lastPairPrice
            prices = prices.slice(prices.size - lengthOfMA, prices.size)
          }
          case Failure(e) => e.printStackTrace
        }
      }
    }*/
  }


  def leastSquares(xSeries: List[Double], ySeries: List[Double]): Array[Double] = {
    val reduceSumFunc = (prev: Double, cur: Double) => prev + cur

    val xBar = xSeries.reduce(reduceSumFunc) * 1.0 / xSeries.length;
    val yBar = ySeries.reduce(reduceSumFunc) * 1.0 / ySeries.length;

    val ssXX = xSeries.map(d => Math.pow(d - xBar, 2)).reduce(reduceSumFunc);
    val ssYY = ySeries.map(d => Math.pow(d - yBar, 2)).reduce(reduceSumFunc);

    val ssXY = xSeries.zipWithIndex.map({ case (d, i) => (d - xBar) * (ySeries(i) - yBar) })
      .reduce(reduceSumFunc);

    val slope = ssXY / ssXX;
    val intercept = yBar - (xBar * slope);
    val rSquare = Math.pow(ssXY, 2) / (ssXX * ssYY);
    
    return Array(slope, intercept, rSquare);
  }
  //"id":198,"last":"0.00417767","lowestAsk":"0.00417800","highestBid":"0.00417767","percentChange":"0.21284885","baseVolume":"772.26246369","quoteVolume":"179405.79588528","isFrozen":"0","high24hr":"0.00499998","low24hr":"0.00339398"
}
