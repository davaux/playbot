package models;

import play.api.Logger
import scala.collection.mutable.ListBuffer

class BotStrategy() {
	var prices: ListBuffer[Double] = ListBuffer.empty[Double]
	// Needed for Momentum Indicator
	var closes: ListBuffer[Double] = ListBuffer.empty[Double]
	val trades: ListBuffer[BotTrade] = ListBuffer.empty[BotTrade]
	var currentPrice = 0.0
	var currentClose = 0.0
	val numSimulTrades = 1
	val indicators = new BotIndicators()

	def tick(candleStick: BotCandleStick): Unit = {
		currentPrice = candleStick.priceAverage
		prices += currentPrice

		// currentClose = candlestick.close
		// closes += currentClose

		Logger.info(s"Price: ${currentPrice}\tMoving Average: ${indicators.movingAverage(prices.toList, 15)}")

		evaluatePositions()

		updateOpenTrades()

		showPositions()
	}

	def evaluatePositions(): Unit = {
		var openTrades = ListBuffer.empty[BotTrade]

		val movingAverage = indicators.movingAverage(prices.toList, 15)

		for(trade <- trades) {
			if(trade.status == "OPEN") openTrades += trade
		}

		if(openTrades.size < numSimulTrades) {
			if(currentPrice < movingAverage) {
				trades += new BotTrade(currentPrice, 0.07) // -7%
			}
		}

		for(trade <- openTrades) {
			if(currentPrice > movingAverage/* && currentPrice >= trade.currentPrice * (1 + 0.1)*/) {
				trade.close(currentPrice)
			}
		}
	}

	def updateOpenTrades(): Unit = {
		for(trade <- trades) {
			if(trade.status == "OPEN") trade.tick(currentPrice)
		}
	}

	def showPositions(): Unit = {
		for(trade <- trades) {
			trade.showTrade()
		}
	}

	def gain(): Double = {
		var total = 0.0
		for(trade <- trades) {
			total += trade.totalGain
		}
		//Logger.info(s"Gain $total")
		total
	}


	def loss(): Double = {
		var total = 0.0
		for(trade <- trades) {
			total += trade.totalLoss
		}
		//Logger.info(s"Loss $total")
		total
	}
}