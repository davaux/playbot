package models

import play.api.Logger

class BotTrade(entryPrice: Double, stopLossAmout: Double) {
	Logger.info("Trade opened")
	var status = "OPEN";
	var exitPrice = 0.0
	val currentPrice = entryPrice
	//val stopLoss = currentPrice - stopLossAmout
	val stopLoss = entryPrice * (1 - stopLossAmout)

	// https://bittrex.com/api/v1.1/market/buylimit?apikey=API_KEY&market=BTC-LTC&quantity=1.2&rate=1.3

	def close(price: Double) = {
		status = "CLOSED"
		exitPrice = price
		Logger.info("Trade closed")
	}

	def showTrade() = {
		var tradeStatus = s"Entry Price: $entryPrice Status: $status Exit Price: $exitPrice"

		if(status == "CLOSED") {
			tradeStatus += " Profit: "
			if(exitPrice > entryPrice) {
				tradeStatus += "\u001b[92m"
			} else {
				tradeStatus += "\u001b[91m"
			}
			tradeStatus += (exitPrice - entryPrice) + "\u001b[0m"
		}
		Logger.info(tradeStatus)
	}

	def tick(currentPrice: Double) = {
		if(currentPrice <= stopLoss) {
			close(currentPrice)
		}
	}

	def totalGain(): Double = {
		var total = 0.0
		if(status == "CLOSED") {
			if(exitPrice > entryPrice) {
				total += (exitPrice - entryPrice)
			}
		}
		total
	}



	def totalLoss(): Double = {
		var total = 0.0
		if(status == "CLOSED") {
			if(exitPrice <= entryPrice) {
				total += (exitPrice - entryPrice)
			}
		}
		total
	}
}