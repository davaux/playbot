package models;

import play.api.Logger
import scala.collection.mutable.ListBuffer

class BotStrategy() {
	var prices: ListBuffer[Double] = ListBuffer.empty[Double]
	val trades: ListBuffer[BotTrade] = ListBuffer.empty[BotTrade]
	var currentPrice = 0.0
	val numSimulTrades = 1
	var totalGain = 0.0
	var totalLoss = 0.0

	val indicators = new BotIndicators()

	def tick(candleStick: ChartData): Unit = {
		currentPrice = candleStick.weightedAverage
		prices += currentPrice

		Logger.info(s"Price: ${candleStick.weightedAverage}\tMoving Average: ${indicators.movingAverage(prices.toList, 15)}")
		evaluatePositions()

		showPositions()

		totalGain += gain()
		totalLoss += loss()
	}

	def evaluatePositions(): Unit = {
		var openTrades = ListBuffer.empty[BotTrade]

		for(trade <- trades) {
			if(trade.status == "OPEN") openTrades += trade
		}

		if(openTrades.size < numSimulTrades) {
			if(currentPrice < indicators.movingAverage(prices.toList, 15)) {
				trades += new BotTrade(currentPrice)
			}
		}

		for(trade <- openTrades) {
			if(currentPrice > indicators.movingAverage(prices.toList, 15)) {
				trade.close(currentPrice)
			}
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
		Logger.info(s"Gain $total")
		total
	}


	def loss(): Double = {
		var total = 0.0
		for(trade <- trades) {
			total += trade.totalLoss
		}
		Logger.info(s"Loss $total")
		total
	}
}