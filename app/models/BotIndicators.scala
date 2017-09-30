package models;

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
}