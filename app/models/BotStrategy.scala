package models;

import play.api.Logger
import scala.collection.mutable.ListBuffer
import controllers.DataPoint

class BotStrategy(botChart: BotChart) {
	var prices: ListBuffer[Double] = ListBuffer.empty[Double]
	var ranges: ListBuffer[Double] = ListBuffer.empty[Double]
	// Needed for Momentum Indicator
	var closes: ListBuffer[Double] = ListBuffer.empty[Double]
	val trades: ListBuffer[BotTrade] = ListBuffer.empty[BotTrade]
	val buys: ListBuffer[DataPoint] = ListBuffer.empty[DataPoint]
	val sells: ListBuffer[DataPoint] = ListBuffer.empty[DataPoint]
	var currentPrice = 0.0
	var currentDate = "";
	var currentClose = 0.0
	val numSimulTrades = 1
	val indicators = new BotIndicators()
	val smaShort = 5
	val smaLong = 15
	var previousPrice = 0.0
	val atrShift = 11
	val atrPeriod = 15
	var accountBalance = botChart.getBalance() // BTC
	var totalAmountPair = 0.0

	def tick(candleStick: BotCandleStick): Unit = {
		currentPrice = candleStick.priceAverage
		currentDate = candleStick.date
		if(!prices.isEmpty) {
			previousPrice = prices.last
		}
		prices += currentPrice

		val trueRange = Math.abs(candleStick.high - candleStick.low)
		ranges += trueRange
		// currentClose = candlestick.close
		// closes += currentClose

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
		val movingAverageA_1 = indicators.movingAverage(prices.toList, smaShort)
		val movingAverageA_2 = indicators.movingAverage(prices.toList, smaShort, 1)
		val movingAverageB_1 = indicators.movingAverage(prices.toList, smaLong)
		val movingAverageB_2 = indicators.movingAverage(prices.toList, smaLong, 1)
		val atrCurrent = indicators.atr(ranges.toList, atrPeriod);
		val atrPast = indicators.atr(ranges.toList, atrPeriod, atrShift);

		for(trade <- trades) {
			if(trade.status == "OPEN") openTrades += trade
		}

		if(prices.size > 2) {
			val limit = accountBalance * 0.05 // 5% of account balance on each trade
			if(limit >= 0.0005) {
				var quantity = currentPrice / limit//1.0//0.0005 / currentPrice

				// ENTRY RULES
				if(openTrades.size < numSimulTrades && quantity > 0) {
					// Rule to ENTER a Long trade
					if(currentPrice <= longMovingAverage) {
						trades += new BotTrade(currentPrice, quantity, 0.07) // -7%
						totalAmountPair += quantity
						accountBalance = accountBalance - (quantity * currentPrice)
						buys += new DataPoint(currentDate, currentPrice.toString, "", "", "")
					}
					//sma40_2 > sma10_2 && sma10_1 >= sma40_1
					/*if(atrCurrent > atrPast) {
						if(movingAverageB_2 > movingAverageA_2 && (movingAverageA_1 >= movingAverageB_1)) {
							trades += new BotTrade(currentPrice, quantity, 0.07) // -7%
							totalAmountPair += quantity
							accountBalance = accountBalance - (quantity * currentPrice)
							buys += new DataPoint(currentDate, currentPrice.toString, "", "", "")
						}
					}*/
				}
			}
		}	

		// EXIT RULES
		for(trade <- openTrades) {
			// Rule to EXIT a Long trade
			if(currentPrice > longMovingAverage) {
				trade.close(currentPrice)
				totalAmountPair -= trade.getQuantity
				accountBalance += trade.getQuantity * currentPrice
				sells += new DataPoint(currentDate, currentPrice.toString, "", "", "")
			}
			//sma10_2 > sma40_2 && sma40_1 >= sma10_1
			/*if(movingAverageA_2 > movingAverageB_2 && (movingAverageB_1 >= movingAverageA_1)) {
				trade.close(currentPrice)
				totalAmountPair -= trade.getQuantity
				accountBalance += trade.getQuantity * currentPrice
				sells += new DataPoint(currentDate, currentPrice.toString, "", "", "")
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
					sells += new DataPoint(currentDate, currentPrice.toString, "", "", "")
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
}