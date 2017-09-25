package models;

class BotIndicators() {
	def movingAverage(dataPoints: List[Double], period: Int): Double = {
		if(dataPoints.length <= 1) 0
		else {
			val dp = dataPoints.slice(dataPoints.size - period, dataPoints.size)
			dp.sum / dp.length
		}

	}
}