package models;

import play.api.Logger
import scala.collection.mutable.ListBuffer

class BotStrategy() {
	var prices: ListBuffer[Double] = ListBuffer.empty[Double]
	var ranges: ListBuffer[Double] = ListBuffer.empty[Double]
	// Needed for Momentum Indicator
	var closes: ListBuffer[Double] = ListBuffer.empty[Double]
	val trades: ListBuffer[BotTrade] = ListBuffer.empty[BotTrade]
	var currentPrice = 0.0
	var currentClose = 0.0
	val numSimulTrades = 1
	val indicators = new BotIndicators()
	val smaShort = 10
	val smaLong = 40
	var previousPrice = 0.0
	val atrShift = 11
	val atrPeriod = 20
	var accountBalance = 0.07 // BTC
	var totalAmountPair = 250.0

	def tick(candleStick: BotCandleStick): Unit = {
		currentPrice = candleStick.priceAverage
		if(!prices.isEmpty) {
			previousPrice = prices.last
		}
		prices += currentPrice

		val trueRange = Math.abs(candleStick.high - candleStick.low)
		ranges += trueRange
		// currentClose = candlestick.close
		// closes += currentClose

		Logger.info(s"Price: ${currentPrice}\tMoving Average X_1: ${indicators.movingAverage(prices.toList, smaShort)}")
		//Logger.info(s"Price: ${currentPrice}\tAverage True Range: ${indicators.atr(ranges.toList, atrPeriod)}")
		//Logger.info(s"Price: ${currentPrice}\tAverage True Range shift: ${indicators.atr(ranges.toList, atrPeriod, atrShift)}")
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

		val currentMovingAverage = indicators.movingAverage(prices.toList, smaShort)
		val movingAverageA_1 = indicators.movingAverage(prices.toList, smaShort, 1)
		val movingAverageA_2 = indicators.movingAverage(prices.toList, smaShort, 2)
		val movingAverageB_1 = indicators.movingAverage(prices.toList, smaLong, 1)
		val movingAverageB_2 = indicators.movingAverage(prices.toList, smaLong, 2)

		for(trade <- trades) {
			if(trade.status == "OPEN") openTrades += trade
		}

		var quantity = 0.0005 / currentPrice

		// ENTRY RULES
		if(openTrades.size < numSimulTrades && quantity > 0) {
			// Rule to ENTER a Long trade
			if(currentPrice < currentMovingAverage && (currentPrice > previousPrice)) {
				trades += new BotTrade(currentPrice, quantity, 0.07, "Long") // -7%
			}
			/*if((movingAverageB_2 > movingAverageA_2) && (movingAverageA_1 >= movingAverageB_1)) {
				trades += new BotTrade(currentPrice, 0.07, "Long") // -7%
			}*/
			// Rule to ENTER a Short trade
			if(currentPrice > currentMovingAverage && (currentPrice < previousPrice)) {
				if(quantity > totalAmountPair) {
					quantity = totalAmountPair
				}
				trades += new BotTrade(currentPrice, quantity, 0.07, "Short") // -7%
			}
			/*if((movingAverageA_2 > movingAverageB_2) && (movingAverageB_1 >= movingAverageA_1)) {
				trades += new BotTrade(currentPrice, 0.07, "Short") // -7%
			}*/
		}

		// EXIT RULES
		for(trade <- openTrades) {
			if(trade.typeOfTrade == "Long") {
				// Rule to EXIT a Long trade
				if(currentPrice > currentMovingAverage) {
					trade.close(currentPrice)
					totalAmountPair += trade.getQuantity
				}
				/*if((movingAverageA_2 > movingAverageB_2) && (movingAverageB_1 >= movingAverageA_1)) {
					trade.close(currentPrice)
				}*/
			} else if(trade.typeOfTrade == "Short") {	
				// Rule to EXIT a Short trade			
				if(currentPrice < currentMovingAverage) {
					trade.close(currentPrice)
					totalAmountPair -= trade.getQuantity
				}
				/*if((movingAverageB_2 > movingAverageA_2) && (movingAverageA_1 >= movingAverageB_1)) {
					trade.close(currentPrice)
				}*/
			}
		}		
	}

	private def updateOpenTrades(): Unit = {
		for(trade <- trades) {
			if(trade.status == "OPEN") trade.tick(currentPrice)
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
		Logger.info(s"Total coin amount: $totalAmountPair")
		Logger.info(s"Hit rate Gain: ${(countGain / trades.size) * 100}% Loss: ${(countLoss / trades.size) * 100}%")
	}
}