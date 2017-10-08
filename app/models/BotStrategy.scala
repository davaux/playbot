package models;

import play.api.Logger
import scala.collection.mutable.ListBuffer
import controllers.DataPoint
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import java.text.SimpleDateFormat
import java.util.Date

class BotStrategy(botChart: BotChart) {
	var prices: ListBuffer[Double] = ListBuffer.empty[Double]
	var ranges: ListBuffer[Double] = ListBuffer.empty[Double]
	// Needed for Momentum Indicator
	var closes: ListBuffer[Double] = ListBuffer.empty[Double]
	var highs: ListBuffer[Double] = ListBuffer.empty[Double]
	var lows: ListBuffer[Double] = ListBuffer.empty[Double]
	var buys: ListBuffer[DataPoint] = ListBuffer.empty[DataPoint]
	var sells: ListBuffer[DataPoint] = ListBuffer.empty[DataPoint]
	val trades: ListBuffer[BotTrade] = ListBuffer.empty[BotTrade]
	val dates: ListBuffer[Long] = ListBuffer.empty[Long]
	var stochastics: ListBuffer[Double] = ListBuffer.empty[Double]
	var currentPrice = 0.0
	var currentDate = "";
	var currentClose = 0.0
	val numSimulTrades = 1
	val indicators = new BotIndicators()
	val smaShort = 4
	val smaLong = 20
	var previousPrice = 0.0
	val atrShift = 11
	val atrPeriod = 14
	var totalAmountPair = 0.0
	var accountBalance = 0.0
	var previousClose = 0.0
  val format = new SimpleDateFormat("yyy-MM-dd HH:mm:ss")

	botChart.getBalance().onComplete {
    		case Success(value) => {
    			accountBalance = value.get
				Logger.info(s"Balance Bittrex: $accountBalance")
    		}
			case Failure(e) => {
				import java.io.StringWriter
				import java.io.PrintWriter
				val sw = new StringWriter
				e.printStackTrace(new PrintWriter(sw))
				Logger.error(sw.toString)
			}
		} // BTC

	def tick(candleStick: BotCandleStick): Unit = {
		currentPrice = candleStick.priceAverage
		currentDate = candleStick.date
		dates += format.parse(currentDate).getTime
		if(!prices.isEmpty) {
			previousPrice = prices.last
		}
		prices += currentPrice

		if(!closes.isEmpty) {
			previousClose = closes.last
		}
		currentClose = candleStick.close
		closes += currentClose
		highs += candleStick.high
		lows += candleStick.low

		val tr1 = candleStick.high - candleStick.low
		val tr2 = Math.abs(candleStick.high - previousClose)
		val tr3 = Math.abs(candleStick.low - previousClose)
		ranges += Math.max(tr1, Math.max(tr2, tr3))

		Logger.info(s"Price: ${currentPrice}\tMoving Average Short: ${indicators.movingAverage(prices.toList, smaShort)}\tMoving Average Long: ${indicators.movingAverage(prices.toList, smaLong)}")
		
		//Logger.info(s"Price: ${currentPrice}\tATR Current: ${indicators.atr(ranges.toList, atrPeriod)}")
		//Logger.info(s"Price: ${currentPrice}\tATR Past: ${indicators.atr(ranges.toList, atrPeriod, atrShift)}")
		/*Logger.info(s"Price: ${currentPrice}\tMoving Average X_2: ${indicators.movingAverage(prices.toList, smaShort, 1)}")
		Logger.info(s"Price: ${currentPrice}\tMoving Average Y_1: ${indicators.movingAverage(prices.toList, smaLong)}")
		Logger.info(s"Price: ${currentPrice}\tMoving Average Y_2: ${indicators.movingAverage(prices.toList, smaLong, 1)}")*/

        			
		evaluatePositions()

		updateOpenTrades()

		showPositions()
	}

	/** 
	 * ENTRY RULES:
     * Enter a long trade when SMA(X) crosses SMA(Y) from bottom
     * Enter a short trade when SMA(X) crosses SMA(Y) from top
     * Exit the long trade when SMA(10) crosses SMA(40) from top
     * Exit the short trade when SMA(10) crosses SMA(40) from bottom
     */
	private def evaluatePositions(): Unit = {
		var openTrades = ListBuffer.empty[BotTrade]

		val shortMovingAverage = indicators.movingAverage(prices.toList, smaShort)
		val longMovingAverage = indicators.movingAverage(prices.toList, smaLong)
		val shortMovingAverage_1 = indicators.movingAverage(prices.toList, smaShort)
		val shortMovingAverage_2 = indicators.movingAverage(prices.toList, smaShort, 2)
		val longMovingAverage_1 = indicators.movingAverage(prices.toList, smaLong)
		val longMovingAverage_2 = indicators.movingAverage(prices.toList, smaLong, 2)
		/*val atrCurrent = indicators.atr(ranges.toList, atrPeriod);
		val atrPast = indicators.atr(ranges.toList, atrPeriod, atrShift);
		val momentum = indicators.momentum(prices.toList)*/


		val percentOfAccountValue = (0.0005 / accountBalance )

		val xLabels = dates.reverse.slice(0, 5).toList;
		val xSeries = for (i <- List.range(1, xLabels.size + 1)) yield i.toDouble

        val ySeries = prices.reverse.slice(0, 5).toList
        val leastSquaresCoeff = leastSquares(xSeries, ySeries);
        val slope = leastSquaresCoeff(0)

        val highSeries = highs.reverse.slice(0, 20).toList
        val linearHighSeries = highSeries.zipWithIndex.map({ case (d, i) => Seq(i, d) })
        val linearHigh = Regression.linear(linearHighSeries, 20)
        val highx1 = xLabels(0);
		val highy1 = linearHigh._1 + linearHigh._2;
		val highx2 = xLabels(xLabels.length - 1);
		val highy2 = linearHigh._1 * highSeries.length + linearHigh._2;
		val highSlope = linearHigh._1
		Logger.info(s"high slope : $highSlope")

        val lowSeries = lows.reverse.slice(0, 20).toList
        val linearLowSeries = lowSeries.zipWithIndex.map({ case (d, i) => Seq(i, d) })
        val linearLow = Regression.linear(linearLowSeries, 20)
        val lowx1 = xLabels(0);
		val lowy1 = linearLow._1 + linearLow._2;
		val lowx2 = xLabels(xLabels.length - 1);
		val lowy2 = linearLow._1 * lowSeries.length + linearLow._2;
		val lowSlope = linearLow._1
		Logger.info(s"low slope : $lowSlope")

        val atr = indicators.atr(ranges.toList, atrPeriod) / currentClose
        val volatilityEntry = previousClose + indicators.atr(ranges.toList, atrPeriod)
        val volatilityExit = previousClose - indicators.atr(ranges.toList, atrPeriod)

        val volatilityRatio = indicators.atr(ranges.toList, atrPeriod) / currentClose
        val rsi = indicators.rsi(closes.toList)
        val tema = indicators.tema(closes.toList)
        val stochastic = indicators.stochastic(currentClose, highs.toList, lows.toList)
        stochastics += stochastic
        val smaStochastic = indicators.movingAverage(stochastics.toList, 3)
        val emaShort = indicators.ema(closes.toList, 5)
        val emaLong = indicators.ema(closes.toList, 15)
        val emaHighs = indicators.ema(highs.toList, 20)
        val emaLows = indicators.ema(lows.toList, 20)

        if(Math.abs(highSlope - lowSlope) < 0.000001) {
        	Logger.info("parallel")
        }


        /*var x1 = xLabels(0);
		var y1 = leastSquaresCoeff(0) + leastSquaresCoeff(1);
		var x2 = xLabels(xLabels.size - 1);
		var y2 = leastSquaresCoeff(0) * xSeries.size + leastSquaresCoeff(1);

		val pX = dates.last
		val pY = currentPrice

		val value = (x2 - x1) * (pY - y1) - (pX - x1) * (y2 - y1)*/
		/*Logger.info(s"Momentum: ${indicators.momentum(prices.toList)}")
		Logger.info(s"leastSquares: ${value}")
		Logger.info(s"Slope : ${slope}")*/

		for(trade <- trades) {
			if(trade.status == "OPEN") openTrades += trade
		}

		Logger.info(s"Percentage of balance $percentOfAccountValue")
		if(percentOfAccountValue < 0.1) {
			val limit = accountBalance * percentOfAccountValue // 5% of account balance on each trade
			if(limit >= 0.0005) {
				var quantity = currentPrice / limit//1.0//0.0005 / currentPrice

				// ENTRY RULES
				if(openTrades.size < numSimulTrades && quantity > 0) {
					// Rule to ENTER a Long trade
					/*if(slope > 0) {
						trades += new BotTrade(currentPrice, quantity, 0.07) // -7%
						totalAmountPair += quantity
						accountBalance = accountBalance - (quantity * currentPrice)
						buys += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
					}*/
					/*if(longMovingAverage_2 > longMovingAverage && slope > 0) {
						trades += new BotTrade(currentPrice, quantity, 0.07) // -7%
						totalAmountPair += quantity
						accountBalance = accountBalance - (quantity * currentPrice)
						buys += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
					}*/
					/*if(currentPrice > volatilityEntry) {
						trades += new BotTrade(currentPrice, quantity, 0.07) // -7%
						totalAmountPair += quantity
						accountBalance = accountBalance - (quantity * currentPrice)
						buys += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
					}*/
					/*if(volatilityRatio > 0.012 && volatilityRatio < 0.02) {
						if(currentPrice < longMovingAverage) {
							trades += new BotTrade(currentPrice, quantity, 0.02) // -7%
							totalAmountPair += quantity
							accountBalance = accountBalance - (quantity * currentPrice)
							buys += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
						}
					}*/
					if(volatilityRatio > 0.005 && volatilityRatio < 0.01) {
						if(currentClose < emaLows) {
							trades += new BotTrade(currentPrice, quantity, 0.07) // -7%
							totalAmountPair += quantity
							accountBalance = accountBalance - (quantity * currentPrice)
							buys += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
						}
					}
					/*if(atr > 0.001 && atr < 0.005) {
						if(Math.abs(highSlope - lowSlope) < 0.000001 && highSlope > 0 && currentPrice <= longMovingAverage) {
							trades += new BotTrade(currentPrice, quantity, 0.07) // -7%
							totalAmountPair += quantity
							accountBalance = accountBalance - (quantity * currentPrice)
							buys += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
						}
					}*/

					/*if(atr > 0.012 && atr < 0.02) {
						if(emaShort > emaLong) {
							trades += new BotTrade(currentPrice, quantity, 0.02) // -7%
							totalAmountPair += quantity
							accountBalance = accountBalance - (quantity * currentPrice)
							buys += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
						}
					}*/
					/*if(rsi >= 70 && smaStochastic < stochastic) {
						trades += new BotTrade(currentPrice, quantity, 0.02) // -7%
						totalAmountPair += quantity
						accountBalance = accountBalance - (quantity * currentPrice)
						buys += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
					}*/
					/*if(currentPrice < longMovingAverage) {
						trades += new BotTrade(currentPrice, quantity, 0.07) // -7%
						totalAmountPair += quantity
						accountBalance = accountBalance - (quantity * currentPrice)
						buys += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
					}*/
					/*if(currentPrice < longMovingAverage) {
						botChart.bittrexBuy(quantity, currentPrice).onComplete {
				    		case Success(value) => {
				    			Logger.info(s"uuid: ${value.get}")
								trades += new BotTrade(currentPrice, quantity, 0.07) // -7%
								totalAmountPair += quantity
								accountBalance = accountBalance - (quantity * currentPrice)
								buys += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
				    		}
							case Failure(e) => {
								import java.io.StringWriter
								import java.io.PrintWriter
								val sw = new StringWriter
								e.printStackTrace(new PrintWriter(sw))
								Logger.error(sw.toString)
							}
						}
					}*/
					/*//sma40_2 > sma10_2 && sma10_1 >= sma40_1
					if(longMovingAverage_2 > shortMovingAverage_2/* && (shortMovingAverage_1 >= longMovingAverage_1)*/) {
						trades += new BotTrade(currentPrice, quantity, 0.07) // -7%
						totalAmountPair += quantity
						accountBalance = accountBalance - (quantity * currentPrice)
						buys += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
					}*/
					/*if(currentClose > volatilityEntry) {
						if(indicators.movingAverage(prices.toList, 15, 2) > indicators.movingAverage(prices.toList, 5, 2) && indicators.movingAverage(prices.toList, 5, 1) >= indicators.movingAverage(prices.toList, 15, 1)) {
							trades += new BotTrade(currentPrice, quantity, 0.07) // -7%
							totalAmountPair += quantity
							accountBalance = accountBalance - (quantity * currentPrice)
							buys += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
						}
					}*/

					/*//(sma10_2 > sma40_2 && sma40_1 >= sma10_1)
					if(shortMovingAverage_2 > longMovingAverage_2 && (longMovingAverage_1 >= shortMovingAverage_1)) {
						trades += new BotTrade(currentPrice, quantity, 0.07) // -7%
						totalAmountPair -= quantity
						accountBalance = accountBalance + (quantity * currentPrice)
						sells += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
					}*/
				}
			}
		}

		// EXIT RULES
		for(trade <- openTrades) {
			// Rule to EXIT a Long trade
			/*if(slope <= 0) {
				trade.close(currentPrice)
				totalAmountPair -= trade.getQuantity
				accountBalance += trade.getQuantity * currentPrice
				sells += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
			}*/
			/*if(longMovingAverage_2 < longMovingAverage && slope < 0) {
				trade.close(currentPrice)
				totalAmountPair -= trade.getQuantity
				accountBalance += trade.getQuantity * currentPrice
				sells += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
			}*/
			/*if(currentPrice > volatilityExit) {
				trade.close(currentPrice)
				totalAmountPair -= trade.getQuantity
				accountBalance += trade.getQuantity * currentPrice
				sells += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
			}*/
			/*if(currentPrice < emaShort) {
				trade.close(currentPrice)
				totalAmountPair -= trade.getQuantity
				accountBalance += trade.getQuantity * currentPrice
				sells += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
			}*/
			/*if(currentPrice < tema) {
				trade.close(currentPrice)
				totalAmountPair -= trade.getQuantity
				accountBalance += trade.getQuantity * currentPrice
				sells += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
			}*/
			if(volatilityRatio < 0.005) {
				if(currentPrice > longMovingAverage) {
					trade.close(currentPrice)
					totalAmountPair -= trade.getQuantity
					accountBalance += trade.getQuantity * currentPrice
					sells += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
				}
			}
			/*if(currentPrice > longMovingAverage) {
				botChart.bittrexSell(trade.getQuantity, currentPrice).onComplete {
		    		case Success(value) => {
		    			Logger.info(s"uuid: ${value.get}")
						trade.close(currentPrice)
						totalAmountPair -= trade.getQuantity
						accountBalance += trade.getQuantity * currentPrice
						sells += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
		    		}
					case Failure(e) => {
						import java.io.StringWriter
						import java.io.PrintWriter
						val sw = new StringWriter
						e.printStackTrace(new PrintWriter(sw))
						Logger.error(sw.toString)
					}
				}
			}*/
			/*if(currentPrice > longMovingAverage/* && currentPrice > trade.entryPrice * (1 + 0.1)*/) {
				trade.close(currentPrice)
				totalAmountPair -= trade.getQuantity
				accountBalance += trade.getQuantity * currentPrice
				sells += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
			}*/
			/*//sma10_2 > sma40_2 && sma40_1 >= sma10_1
			if(shortMovingAverage_2 > longMovingAverage_2/* && (longMovingAverage_1 >= shortMovingAverage_1)*/) {
				trade.close(currentPrice)
				totalAmountPair -= trade.getQuantity
				accountBalance += trade.getQuantity * currentPrice
				sells += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
			}*/
			/*if(currentClose > volatilityExit) {
				if(indicators.movingAverage(prices.toList, 5, 2) > indicators.movingAverage(prices.toList, 15, 2) && indicators.movingAverage(prices.toList, 15, 1) >= indicators.movingAverage(prices.toList, 5, 1)) {
					trade.close(currentPrice)
					totalAmountPair -= trade.getQuantity
					accountBalance += trade.getQuantity * currentPrice
					sells += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
				}
			}*/
		}		
	}

	private def updateOpenTrades(): Unit = {
		for(trade <- trades) {
			if(trade.status == "OPEN") {
				trade.tick(currentPrice)
				if(trade.status == "CLOSED") {
					Logger.info("Stop Loss reached")
					totalAmountPair -= trade.getQuantity
					accountBalance += trade.getQuantity * currentPrice
					sells += new DataPoint(currentDate, currentPrice.toString, "", "", "", "", "", "", "")
				}
			}
		}
	}

	private def showPositions(): Unit = {
		for(trade <- trades) {
			trade.showTrade()
		}
	}

	def profit(): Double = {
		var total = 0.0
		for(trade <- trades) {
			total += trade.profit()
		}
		total
	}

	def hitrate(): Unit = {
		var countGain = 0.0
		var countLoss = 0.0
		for(trade <- trades) {
			if(trade.profit() >= 0.0) {
				countGain += 1.0
			} else if(trade.profit() < 0.0) {
				countLoss += 1.0
			}
		}
		Logger.info(s"Total coin: $totalAmountPair")
		Logger.info(s"Account Balance: $accountBalance")
		Logger.info(s"Hit rate Gain: ${(countGain / trades.size) * 100}% Loss: ${(countLoss / trades.size) * 100}%")
	}

	def getBuys(): List[DataPoint] = {
		buys.toList
	}

	def getSells(): List[DataPoint] = {
		sells.toList
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