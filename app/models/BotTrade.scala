package models

import play.api.Logger

class BotTrade(entryPrice: Double, quantity: Double, stopLossAmout: Double) {
	Logger.info(s"Trade opened")
	var status = "OPEN";
	var exitPrice = 0.0
	//val stopLoss = currentPrice - stopLossAmout

	// https://bittrex.com/api/v1.1/market/buylimit?apikey=API_KEY&market=BTC-LTC&quantity=1.2&rate=1.3

	def close(price: Double) = {
		status = "CLOSED"
		exitPrice = price
		Logger.info(s"Trade closed")
	}

	def showTrade() = {
		var tradeStatus = s"Entry Price: $entryPrice Quantity: $quantity Status: $status Exit Price: $exitPrice"

		if(status == "CLOSED") {
			tradeStatus += " Profit: "
			if(exitPrice >= entryPrice) {
				tradeStatus += "\u001b[92m"
			} else {
				tradeStatus += "\u001b[91m"
			}
			tradeStatus += ((exitPrice - entryPrice) * quantity) + "\u001b[0m"
		}
		Logger.info(tradeStatus)
	}

	def tick(currentPrice: Double) = {
		val stopLoss = entryPrice * (1 - stopLossAmout)
		if(currentPrice <= stopLoss) {
			close(currentPrice)
		}
	}

	def profit(): Double = {
		var profit = 0.0
		if(status == "CLOSED") {
			profit = ((exitPrice - entryPrice) * quantity)
		}
		profit
	}

	def getQuantity(): Double = {
		quantity
	}
}