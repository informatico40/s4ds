import breeze.linalg._
import breeze.stats._
import breeze.optimize._

import scala.io.Source
import scala.reflect.ClassTag
import breeze.numerics._
//here copy the absolute path for test purpose
val file = Source.fromFile("/home/ernesto/Documents/s4ds/chap02/data/rep_height_weights.csv")
val lines = file.getLines.toVector
val splitLines = lines.map { _.split(',') }

def fromList[T:ClassTag](index:Int, converter:(String => T)):DenseVector[T] =
  DenseVector.tabulate(lines.size) { irow => converter(splitLines(irow)(index)) }

val genders = fromList(1, elem => elem.replace("\"", "").head)
val weights = fromList(2, elem => elem.toDouble)
val heights = fromList(3, elem => elem.toDouble)
val reportedWeights = fromList(4, elem => elem.toDouble)
val reportedHeights = fromList(5, elem => elem.toDouble)


val npoints = heights.length
require(weights.length == npoints)
require(reportedWeights.length == npoints)
require(genders.length == npoints)
require(reportedHeights.length == npoints)


heights - mean(heights)

val maleVector = DenseVector.fill(genders.length) ('M')
val isMale = genders :== maleVector
heights(isMale).toDenseVector
sum(I(isMale))

val rel = genders.values.map(c => if (c == 'M') 1.0 else 0.0)

def gradf(xs:DenseVector[Double]) = 2.0 :* xs
def f(xs:DenseVector[Double]) = sum(xs :^ 2.0)
f(rel)


val optTrait = new DiffFunction[DenseVector[Double]] {
  def calculate(xs:DenseVector[Double]) = (f(xs), gradf(xs))
}

val minimum = minimize(optTrait,weights)
minimum.length

/*lazy val rescaledHeights:DenseVector[Double] =
  (heights - mean(heights)) / stddev(heights)

lazy val rescaledWeights:DenseVector[Double] =
  (weights - mean(weights)) / stddev(weights)

lazy val featureMatrix:DenseMatrix[Double] =
  DenseMatrix.horzcat(
    DenseMatrix.ones[Double](npoints, 1),
    rescaledHeights.toDenseMatrix.t,
    rescaledWeights.toDenseMatrix.t
  )*/

/*
val lines = Array("\"1\",\"M\",77,182,77,180","\"2\",\"M\",77,182,77,180","\"3\",\"M\",77,182,77,180")
val splitLine = lines.map(_.split(","))

lines(0)(0)
*/

/*
def convert[T: ClassTag](index: Int, f: String => T ): DenseVector[T] = {
  DenseVector.tabulate(lines.size){irow => f(lines(irow)(index))}
}



val a = convert(1, el => el.replace("\"", "").head)


convert(2, el => el.toDouble)
convert(3, el => el.toDouble)
convert(4, el => el.toDouble)
*/

/*
def fromList[T:ClassTag](index:Int, converter:(String => T)):DenseVector[T] =
  DenseVector.tabulate(1) { irow => converter(splitLine(irow)(index)) }
*/





/*val m1 = DenseMatrix((2.0, 3.0), (5.0, 6.0), (8.0, 9.0))
val m2 = DenseMatrix((10.0, 11.0), (12.0, 13.0))
val v = DenseVector(1.0, 2.0)

m1 * m2
m1 * v

DenseMatrix((10, 11), (12, 13)) * DenseVector(1,2)

DenseMatrix((1, 2), (2, 3), (4, 3)) * DenseMatrix((10, 11, 1), (12, 13, 1))*/

/*val v = DenseVector(1.0, 2.0, 3.0)
val v2 = DenseVector(4.0, 5.0, 6.0)

val m = DenseMatrix((1.0, 2.0, 3.0), (4.0, 5.0, 6.0))
2.0 :* m

DenseVector.ones[Double](5)
DenseVector.zeros[Int](5)
linspace(0.0,1.0,3)

DenseVector.tabulate(4){ i => 5.0 * i}


DenseMatrix.tabulate[Int](2, 3) {
  (irow, icol) => irow*2 + icol
}

DenseVector.rand(2)
DenseMatrix.rand(2,3)

val arr = Array(2, 3, 4, 5, 6)
val dense = DenseVector(arr :_  *)

dense(-1)
dense(-2)

dense(1 to 3)
dense(dense.length-1 to 0 by -2)
val vSlice = dense(1,3)

val mask = DenseVector(true, false, false, true, true)
dense(mask).toDenseVector
dense(dense :> 3).toDenseVector*/

/*v(0 until 2) := 0.0*/
/*v :+= 4.0
v*/
/*
v :+ v2
v :^ v2
v dot v2
v + v2

(v + v2) :* 2.0
2.0 :* v :+ v2*/
