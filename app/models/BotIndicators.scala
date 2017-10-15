package models;

import scala.collection.mutable.ListBuffer
import play.api.Logger

class BotIndicators() {
	def movingAverage(dataPoints: List[Double], period: Int, shift: Int = 0): Double = {
		if(dataPoints.length <= 1) 0
		else {
			val dp = dataPoints.slice(dataPoints.size - period - shift, dataPoints.size - 1 - shift)
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
			// RSI is above 70 and we own, sell

			val value = 100 - 100 / (1 + rs)
	        if (value > 65) {
	            return -1;
	        // RSI is below 30, buy
	        } else if (value < 35) {
	            return 1;
	        } else {
	            return 0;
	        }
		}
	}

	def ema(dataPoints: List[Double], period: Int = 14, shift: Int = 0): Double = {
		if(dataPoints.size < period + 1) 0
		def calculate(todaysPrice: Double, emaPast: Double): Double = {
			val k = 2 / (period + 1)
			todaysPrice * k + emaPast * (1 - k)
		}
		val dps = dataPoints.slice(dataPoints.size - period - shift, dataPoints.size - shift)
		var ema = movingAverage(dps, period, 1)
		for(dataPoint <- dps) {
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

	def adx(trueRanges: List[Double], lows: List[Double], highs: List[Double], closes: List[Double], period: Int = 14): Double = {
		val tr: ListBuffer[Double] = ListBuffer.empty[Double]
		val dmPlus: ListBuffer[Double] = ListBuffer.empty[Double]
		val dmMinus: ListBuffer[Double] = ListBuffer.empty[Double]
		val trN: ListBuffer[Double] = ListBuffer.empty[Double]
		val dmPlusN: ListBuffer[Double] = ListBuffer.empty[Double]
		val dmMinusN: ListBuffer[Double] = ListBuffer.empty[Double]
		val dx: ListBuffer[Double] = ListBuffer.empty[Double]
		val adx: ListBuffer[Double] = ListBuffer.empty[Double]
		var counter = 0

		// int periodStart = qh.size() - period; 
		val periodEnd = trueRanges.size - 1; 
		val high = highs.last; 
		val low = lows.last; 
		// double close = qh.getLastPriceBar().getClose(); 
		val high_1 = highs(highs.size - 2) 
		val low_1 = lows(lows.size - 2); 
		val close_1 = closes(periodEnd - 1)

		// the first calculation for ADX is the true range value (TR) 
		tr(period - 1) = Math.max(high - low, Math.max(Math.abs(high - close_1), Math.abs(low - close_1)));

		// determines the positive directional movement or returns zero if there 
		// is no positive directional movement. 
		dmPlus(period - 1) = if (high - high_1 > low_1 - low) Math.max(high - high_1, 0) else 0.0;


		// calculates the negative directional movement or returns zero if there 
		// is no negative directional movement. 
		dmMinus(period - 1) = if (low_1 - low > high - high_1) Math.max(low_1 - low, 0) else 0.0;

		// The daily calculations are volatile and so the data needs to be 
		// smoothed. First, sum the last N periods for TR, +DM and - DM 
		var trSum = 0.0; 
		var dmPlusSum = 0.0; 
		var dmMinusSum = 0.0; 
		for (i <- 0 to period) { 
			trSum += tr(i); 
			dmPlusSum += dmPlus(i); 
			dmMinusSum += dmMinus(i); 
		}

		// The smoothing formula subtracts 1/Nth of yesterday's trN from 
		// yesterday's trN and then adds today's TR value 
		// The truncating function is used to calculate the indicator as close 
		// as possible to the developer of the ADX's original form of 
		// calculation (which was done by hand). 
		trN(period - 1) = (1000 * (trN(period - 2) - (trN(period - 2) / period) + trSum)) / 1000; 
		dmPlusN(period - 1) = (1000 * (dmPlusN(period - 2) - (dmPlusN(period - 2) / period) + dmPlusSum)) / 1000; 
		dmMinusN(period - 1) = (1000 * (dmMinusN(period - 2) - (dmMinusN(period - 2) / period) + dmMinusSum)) / 1000

		// Now we have a 14-day smoothed sum of TR, +DM and -DM. 
		// The next step is to calculate the ratios of +DM and -DM to TR. 
		// The ratios are called the +directional indicator (+DI) and 
		// -directional indicator (-DI). 
		// The integer function (int) is used because the original developer 
		// dropped the values after the decimal in the original work on the ADX 
		// indicator. 
		val diPlus = 100 * dmPlusN(period - 1) / trN(period - 1)
		val diMinus = 100 * dmMinusN(period - 1) / trN(period - 1)

		// The next step is to calculate the absolute value of the difference 
		// between the +DI and the -DI and the sum of the +DI and -DI. 
		val diDiff = Math.abs(diPlus - diMinus)
		val diSum = diPlus + diMinus

		// The next step is to calculate the DX, which is the ratio of the 
		// absolute value of the difference between the +DI and the -DI divided 
		// by the sum of the +DI and the -DI. 
		dx(period - 1) = 100 * (diDiff / diSum)

		// The final step is smoothing the DX to arrive at the value of the ADX. 
		// First, average the last N days of DX values 
		var dxMedia = 0.0; 
		for (i <- 0 to period) { 
			dxMedia += dx(i); 
		} 
		dxMedia /= period

		// The smoothing process uses yesterday's ADX value multiplied by N-1, 
		// and then add today's DX value. Finally, divide this sum by N. 
		if (counter == 2 * (period - 1)) { 
			adx(period - 2) = dxMedia; 
		} 
		adx(period - 1) = (adx(period - 2) * (period - 1) + dx(period - 1)) / period 

		counter += 1

		val value = adx(period - 1)

		if (value > 50) {
            return -1; // overbought
        } else if (value < 20) {
            return 1;  // underbought
        } else {
            return 0;
        }
	}

	def macd(dataPoints: List[Double], shortPeriod: Int = 12, longPeriod: Int = 26): Int = {
		var macdValues: ListBuffer[Double] = ListBuffer.empty[Double]
		for (i <- 0 to 10) {
			val shortEMA = ema(dataPoints, shortPeriod, i)
			val longEMA = ema(dataPoints, longPeriod, i)
			macdValues += shortEMA - longEMA
		}
		val emaMacd = ema(macdValues.toList, 9)
		val value = macdValues(0) - emaMacd
		// Close position for the pair when the MACD signal is negative
        if (value < 0) {
            -1;
        // Enter the position for the pair when the MACD signal is positive
        } else if (value > 0) {
            1;
        } else {
            0;
        }
	}

	def awesomeOscillator(dataPoints: List[Double]): Int = {
		val aoFast1 = movingAverage(dataPoints, 5)
        val aoSlow1 = movingAverage(dataPoints, 34)
		val aoFast2 = movingAverage(dataPoints, 5, 1)
        val aoSlow2 = movingAverage(dataPoints, 34, 1)
        val aoPrior = aoFast2 - aoSlow2
        val ao = aoFast1 - aoSlow1
		/** Bullish cross */
        if (aoPrior <= 0 && ao > 0) {
            100;
        /** Bearish cross */
        } else if(aoPrior >= 0 && ao < 0){
            -100;
        } else {
            0;
        }
	}
}