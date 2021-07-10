import scala.util.Random.{nextDouble, nextInt}
import org.scalameter._
import scala.collection.parallel._
import scala.collection.parallel.CollectionConverters._
import scala.math.BigDecimal
import scala.math.abs

import scala.io.Source

object csvReader {
  def readIris() = {
    val s = Source.fromFile("data/iris.csv").getLines().map(l => l.split(","))
      .map(l => l.slice(0, 4))
    s.map(l => l.map(_.toDouble)).toArray
  }
}

object Example {

  def cluster(points: Array[Array[Double]], initialCentroids: Array[Array[Double]],
              epsilon: Double = 0.0001, maxIterations: Int = 100) = {

    var size = points.length
    var dimensions = points(0).length
    var k = initialCentroids.length
    println(s"Clustering $size data points ($dimensions dimensions) into $k groups")

    var SSE = Double.MaxValue
    var done = false
    var iterations = 0
    var centroids = initialCentroids.clone

    while (!done && iterations < maxIterations) {
      var distances = new Array[Double](0)
      var recomputeCentroids = new Array[Array[Double]](0)
      val g1 = points.groupBy(x => nearestCentroid(x, centroids))

      for ((centroid, points) <- g1) {
        recomputeCentroids +:= average(points)
        distances :+= sumSquareDistance(centroid, points)
      }
      centroids = recomputeCentroids
      iterations += 1

      var p = 0
 			var currSSE = 0.0
      while (p < k) {
        currSSE += distances(p)
        p += 1
      }

      if (abs(currSSE - SSE) < epsilon) {
        done = true
      }
			SSE = currSSE
    }
    centroids
  }

  def sumSquareDistance(centroid: Array[Double], points: Array[Array[Double]]): Double = {
    var sum = 0.0
    points.foreach(point => {
			sum += squareDistance(point, centroid)
		})
    sum
  }

  def average(points: Array[Array[Double]]) = {
    var accumulated = new Array[Double](4)

    for (point <- points) {
      for (i <- 0 until point.length) {
        accumulated(i) = accumulated(i) + point(i)
      }
    }

    for (i <- 0 until accumulated.length) {
      accumulated(i) = BigDecimal(accumulated(i) / points.length).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble
    }

    accumulated
  }

  def initialCentroids(k: Int, data: Array[Array[Double]]): Array[Array[Double]] = {
    val centroids = new Array[Array[Double]](k)
    for (i <- 0 until k) {
      centroids(i) = data(nextInt(data.length))
    }
    centroids
  }

  def squareDistance(p: Array[Double], c: Array[Double]): Double = {
    var d = 0.0
    for (i <- 0 until p.length) {
      val t = p(i) - c(i)
      d += t * t
    }
    d
  }

  def nearestCentroid(p: Array[Double], centroids: Array[Array[Double]]): Array[Double] = {
    var nc = centroids(0)
    var d = squareDistance(p, centroids(0))
    for (i <- 1 until centroids.length) {
      val dt = squareDistance(p, centroids(i))
      if (dt < d) {
        d = dt
        nc = centroids(i)
      }
    }
    return nc
  }

  def strPoint(p: Array[Double]): String = {
    "<" + p.mkString(",") + ">"
  }

  def main(args: Array[String]): Unit = {
    val data = csvReader.readIris()
    val centroids = initialCentroids(3, data)
    val t1 = System.nanoTime

    var time = config(
      Key.exec.benchRuns -> 1,
      Key.verbose -> true
    ) withWarmer {
      new Warmer.Default
    } withMeasurer {
      new Measurer.IgnoringGC
    } measure {
			cluster(data, centroids)
    }
    println(s"Time: $time")
  }

}
