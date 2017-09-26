package models;

import play.api.Logger
import java.util.Calendar
import java.text.SimpleDateFormat

class BotCandleStick(period: Int = 300) {
	var current: Double = 0.0
	var open: Double = 0.0
	var close: Double = 0.0
	private var closed: Boolean = false
	var high: Double = 0.0
	var low: Double = 0.0
	var startTime = Calendar.getInstance()
	var priceAverage: Double = 0.0

	def tick(price: Double): Unit = {
		current = price
		if(open <= 0) open = current
		if((high <= 0) || (current > high)) high = current
		if((low <= 0) || (current < low)) low = current
		val startTimePlusPeriod = Calendar.getInstance
		startTimePlusPeriod.setTime(startTime.getTime)
		startTimePlusPeriod.add(Calendar.SECOND, period)
		val now  = Calendar.getInstance
		
		if(now.after(startTimePlusPeriod) || now.equals(startTimePlusPeriod)) {
			closed = true;
			close = current
			priceAverage = (high + low + close) / 3.0
		}

		Logger.debug(s"Open: $open Close: $close High: $high Low: $low Current: $current")
	}

	def isClosed(): Boolean = closed
}