package models;

import play.api.Logger
import scala.collection.mutable.ListBuffer

class BotBacktestStrategy() {
	var prices: ListBuffer[Double] = ListBuffer.empty[Double]
	val trades: ListBuffer[BotTrade] = ListBuffer.empty[BotTrade]
	var currentPrice = 0.0
	val numSimulTrades = 1
	val indicators = new BotIndicators()

	def tick(candleStick: ChartData): Unit = {
		currentPrice = candleStick.weightedAverage
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
				trades += new BotTrade(currentPrice, 0.0001)
			}
		}

		for(trade <- openTrades) {
			if(currentPrice > movingAverage) {
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