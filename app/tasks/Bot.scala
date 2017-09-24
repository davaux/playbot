package tasks

import javax.inject.{Inject, Named}

import akka.actor.{ActorRef, ActorSystem}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import java.text.SimpleDateFormat
import java.util.Calendar


class CodeBlockTask @Inject() (actorSystem: ActorSystem)(implicit executionContext: ExecutionContext) {

	var periods = Array(300, 900, 1800, 7200, 14400, 86400)
	val format = new SimpleDateFormat("yyy-MM-dd HH:mm:ss")

	actorSystem.scheduler.schedule(initialDelay = 10.seconds, interval = 10.seconds) {
		// the block of code that will be executed
		println(format.format(Calendar.getInstance().getTime()) + " period...")
	}
}