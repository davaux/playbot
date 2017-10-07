package models;

import scala.collection.mutable.ListBuffer

class BotIndicators() {
	def movingAverage(dataPoints: List[Double], period: Int, shift: Int = 0): Double = {
		if(dataPoints.length <= 1) 0
		else {
			val dp = dataPoints.slice(dataPoints.size - period - shift, dataPoints.size - shift)
			dp.sum / dp.length
		}

	}

	def atr(dataPoints: List[Double], period: Int, shift: Int = 0): Double = {
		val dp = dataPoints.slice(dataPoints.size - period - shift, dataPoints.size - shift)
		dp.sum / dp.length
	}

	def momentum (dataPoints: List[Double], period: Int = 14): Double = {
		if (dataPoints.size > period - 1)
			return dataPoints.last * 100 / dataPoints.reverse(period - 1)
		else 0.0
	}

	def rsi(dataPoints: List[Double], period: Int = 14): Double = {
		if(dataPoints.size < period + 1) 50
		else {
			// Calculate up moves down moves
			var ups: ListBuffer[Double] = ListBuffer.empty[Double]
			var downs: ListBuffer[Double] = ListBuffer.empty[Double]
			for (i <- dataPoints.size - period to dataPoints.size - 1) {
				val change = dataPoints(i) - dataPoints(i - 1)
				val up = if (change > 0) change else 0
				val down = if(change < 0) Math.abs(change) else 0
				ups += up
				downs += down
			}
			// Averaging
			val avgUps = ups.sum / period
			val avgDowns = downs.sum / period
			// Calculate Relative Strength
			val rs = avgUps / avgDowns
			100 - 100 / (1 + rs)
		}
	}

	def ema(dataPoints: List[Double], period: Int = 14): Double = {
		if(dataPoints.size < period + 1) 0
		def calculate(todaysPrice: Double, emaPast: Double): Double = {
			val k = 2 / (period + 1)
			todaysPrice * k + emaPast * (1 - k)
		}
		var ema = movingAverage(dataPoints.slice(dataPoints.size - period - 1, dataPoints.size - 2), period)
		for(dataPoint <- dataPoints) {
			ema = calculate(dataPoint, ema)
		}
		ema

	}

	def tema(dataPoints: List[Double], period: Int = 14) = {
		val e: Double = ema(dataPoints, period)
		(3 * e) - (3 * e * e) + (e * e *e)
	}

	def stochastic(current: Double, lows: List[Double], highs: List[Double], period: Int = 14) = {
		if(lows.size < period) 0
		else if(highs.size < period) 0
		else {
			val minLow = lows.slice(lows.size - period, lows.size - 1).min
			(current - minLow) / (highs.slice(highs.size - period, highs.size - 1).max - minLow) * 100
		}
	}
}