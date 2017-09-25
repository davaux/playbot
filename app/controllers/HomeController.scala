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

  val periods = Array(300, 900, 1800, 7200, 14400, 86400)
  val format = new SimpleDateFormat("yyy-MM-dd HH:mm:ss")
  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def trade(pair: String) = Action.async { implicit request: Request[AnyContent] =>
    import scala.concurrent.ExecutionContext.Implicits.global
    
    val botChart = new BotChart("poloniex", pair, periods(3))
    val strategy = new BotStrategy();
    val historicalData = botChart.data(ws);
    var dataPoints: ListBuffer[DataPoint] = new ListBuffer[DataPoint]()

    historicalData.map(value => 
      {
        for(candleStick <- value.get) {
          val lastPairPrice = candleStick.weightedAverage
          val dataDate = format.format(candleStick.date * 1000L)
          strategy.tick(candleStick)
          //Logger.info("" + candleStick)

          dataPoints += new DataPoint(dataDate, lastPairPrice.toString, "", "", "")
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
        Logger.info(s"Total Gain : ${strategy.gain()}")
        Logger.info(s"Total Loss : ${strategy.loss()}")
        Ok(views.html.trade(dataPoints.toList))
      }

    )
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
  
}
