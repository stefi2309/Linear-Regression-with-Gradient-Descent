type Mat = List[List[Double]]

// in cadrul acestei implementari am folosit ca suport cursul si laboratorul

class Matrix(m: Option[List[List[Double]]]) {
  private def transpose_helper(matrix: Mat): Mat =
    matrix match
      case Nil :: _ => Nil
      case _ => matrix.map(_.head) :: transpose_helper(matrix.map(_.tail))

  def transpose: Matrix =
    m match
      case None => Matrix(None)
      case Some(matrix) => Matrix(Some(transpose_helper(matrix)))

  // functie ajutatoare pentru a face map pe o lista
  private def mapList(line: List[Double], f: Double => Double): List[Double] =
    line match
      case head :: tail => f(head) :: mapList(tail, f)
      case Nil => Nil

  // functie ajutatoare pentru a face map pe o matrice
  private def mapMatrix(matrix: List[List[Double]], f: Double => Double): List[List[Double]] =
    matrix match
      case head :: tail => mapList(head, f) :: mapMatrix(tail, f)
      case Nil => Nil

  def map(f: Double => Double): Matrix =
    m match
      case None => Matrix(None)
      case Some(matrix) => Matrix(Some(mapMatrix(matrix,f)))

  def *(other: Matrix): Matrix =
    m match
      case None => Matrix(None)
      case Some(matrix) =>
        other.data match
          case None => Matrix(None)
          case Some(matrix2) =>
            if (width != other.height)
              Matrix(None)
            else {
              Matrix(Some(matrix.map(line =>
                matrix2.transpose.map(line2 =>
                  line.zip(line2).map(elem => elem._1 * elem._2).foldLeft(0.0)(_ + _)))))
            }

  def ++(x: Double): Matrix =
    m match
      case None => Matrix(None)
      case Some(matrix) =>
        Matrix(Some(matrix.map(line => line ++ List(x))))

  def -(other: Matrix): Matrix =
    m match
      case None => Matrix(None)
      case Some(matrix) =>
        other.data match
          case None => Matrix(None)
          case Some(matrix2) =>
            if(height != other.height || width != other.width)
              Matrix(None)
            else
              Matrix(Some(matrix.zip(matrix2).map(pair =>
                pair._1.zip(pair._2).map(elem => elem._1 - elem._2))))

  def data: Option[Mat] = m

  def height: Option[Int] =
    m match
      case Some(matrix) => Some(matrix.length)
      case None => None

  def width: Option[Int] =
    m match
      case Some(matrix) => Some(matrix.head.length)
      case None => None

  // am implementat functiile ajutatoare listToString si matrixToString
  override def toString: String =
    def listToString(sep: String)(list: List[Double]): String =
      def op(elem: Double, acc: String): String = {
        if (acc.isEmpty) elem.toString
        else elem.toString + sep + acc
      }
      list.foldRight("")(op)

    def matrixToString(sep: String, matrix: List[List[Double]]): String =
      def op(line: List[Double], acc: String): String = {
        if (acc.isEmpty) listToString(",")(line)
        else listToString(",")(line) ++ sep + acc
      }
      matrix.foldRight("")(op)

    m match
      case Some(matrix) => matrixToString("\n",matrix)
      case None => "Gol"
}

object Matrix {
  def apply(data: Mat): Matrix = new Matrix(Some(data))
  def apply(data: Option[Mat]): Matrix = new Matrix(data)
  def apply(dataset: Dataset): Matrix =
    new Matrix(Some(dataset.data.tail.map(_.map(_.toDouble))))
}
