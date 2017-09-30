package models

import play.api.Logger

class BotTrade(price: Double, quantity: Double, stopLossAmout: Double, val typeOfTrade: String) {
	var status = "OPEN";
	Logger.info(s"Trade opened")
	var exitPrice = 0.0
	val entryPrice = price * quantity
	//val stopLoss = currentPrice - stopLossAmout
	Logger.info(s"quantity $quantity")

	// https://bittrex.com/api/v1.1/market/buylimit?apikey=API_KEY&market=BTC-LTC&quantity=1.2&rate=1.3

	def close(price: Double) = {
		status = "CLOSED"
		exitPrice = price * quantity
		Logger.info(s"Trade closed")
	}

	def showTrade() = {
		var tradeStatus = s"$typeOfTrade Trade Entry Price: $entryPrice Status: $status Exit Price: $exitPrice"

		if(status == "CLOSED") {
			tradeStatus += " Profit: "
			if(typeOfTrade == "Long") {
				if(exitPrice >= entryPrice) {
					tradeStatus += "\u001b[92m"
				} else {
					tradeStatus += "\u001b[91m"
				}
				tradeStatus += (exitPrice - entryPrice) + "\u001b[0m"
			} else if(typeOfTrade == "Short") {
				if(exitPrice <= entryPrice) {
					tradeStatus += "\u001b[92m"
				} else {
					tradeStatus += "\u001b[91m"
				}
				tradeStatus += (entryPrice - exitPrice) + "\u001b[0m"
			}
		}
		Logger.info(tradeStatus)
	}

	def tick(currentPrice: Double) = {
		if(typeOfTrade == "Long") {
			val stopLoss = price * (1 - stopLossAmout)
			if(currentPrice <= stopLoss) {
				close(currentPrice)
			}
		} else if(typeOfTrade == "Short") {
			val stopLoss = price * (1 + stopLossAmout)
			if(currentPrice >= stopLoss) {
				close(currentPrice)
			}
		}
	}

	def profit(): Double = {
		var profit = 0.0
		if(status == "CLOSED") {
			if(typeOfTrade == "Long") {
				profit = exitPrice - entryPrice
			} else if (typeOfTrade == "Short") {
				profit = entryPrice - exitPrice
			}
		}
		profit
	}

	def getQuantity(): Double = {
		quantity
	}
}