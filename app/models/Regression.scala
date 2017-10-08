package models

import scala.math.{pow}
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.collection.mutable.ArrayBuffer

object Regression {
  def linear(pairs: List[Seq[Double]], period: Int = 20) = {
    val n = pairs.size
    if (n < period) (0.0, 0.0, 0.0)
    else {

      val sums = for {
        sumXi <- Future {
          var sum = 0.0
          for (pair <- pairs) sum += pair(0)
          sum
        }
        sumYi <- Future {
          var sum = 0.0
          for (pair <- pairs) sum += pair(1)
          sum
        }
        sumX2i <- Future {
          var sum = 0.0
          for (pair <- pairs) sum += pow(pair(0), 2)
          sum
        }
        sumY2i <- Future {
          var sum = 0.0
          for (pair <- pairs) sum += pow(pair(1), 2)
          sum
        }
        sumXYi <- Future {
          var sum = 0.0
          for (pair <- pairs) sum += pair(0) * pair(1)
          sum
        }

      } yield (sumXi, sumYi, sumX2i, sumY2i, sumXYi)

      val (sumX, sumY, sumX2, sumY2, sumXY) = Await.result(sums, Duration.Inf)

      val dn = n * sumX2 - pow(sumX, 2)
      assert(dn != 0.0, "Can't solve the system!")

      val poms = for {
        slopei <- Future {
          ((n * sumXY) - (sumX * sumY)) / dn
        }
        intercepti <- Future {
          ((sumY * sumX2) - (sumX * sumXY)) / dn
        }
        t1i <- Future {
          ((n * sumXY) - (sumX * sumY)) * ((n * sumXY) - (sumX * sumY))
        }
        t2i <- Future {
          (n * sumX2) - pow(sumX, 2)
        }
        t31 <- Future {
          (n * sumY2) - pow(sumY, 2)
        }

      } yield (slopei, intercepti, t1i, t2i, t31)

      val (slope, intercept, t1, t2, t3) = Await.result(poms, Duration.Inf)

      if (t2 * t3 != 0.0)
        (slope, intercept, t1 / (t2 * t3))
      else
        (slope, intercept, 0.0)
    }
  }
}