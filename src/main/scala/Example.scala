import scala.util.Random.{nextDouble, nextInt}
import org.scalameter._
import scala.collection.parallel._
import scala.collection.parallel.CollectionConverters._

import scala.io.Source

object csvReader {
  def readIris() = {
    val s = Source.fromFile("data/iris.csv").getLines().map(l => l.split(","))
      .map(l => l.slice(0, 4))
    s.map(l => l.map(_.toDouble)).toArray
  }
}

object Example {

  def initialCentroids(k: Int, data: Array[Array[Double]]): Array[Array[Double]] = {
    val centroids = new Array[Array[Double]](k)
    for (i <- 0 until k) {
      centroids(i) = data(nextInt(data.length))
    }
    centroids
  }

  def squareDistance(p: Array[Double], c: Array[Double]) : Double = {
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
    println("Hola Mundo")
    val data = csvReader.readIris()
    val centroids = initialCentroids(3, data)

    val g1 = data.groupBy(x => nearestCentroid(x, centroids))

    for((centroid, points) <- g1) {
      println(strPoint(centroid))
      for(p <- points) {
        println("\t" + strPoint(p))
      }
    }
  }

}
