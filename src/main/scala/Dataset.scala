import scala.annotation.tailrec
import scala.io.Source

class Dataset(m: List[List[String]]) {
  val data: List[List[String]] = m
  override def toString: String =
    def listToString(sep: String)(list: List[String]): String = {
      def op(elem: String, acc: String): String = {
        if (acc.isEmpty) elem
        else elem + sep + acc
      }
      list.foldRight("")(op)
    }
    listToString("\n")(data.map(listToString(",")))

  // functie pentru determinarea indexului unei coloane din antet
  private def searchIndex(list: List[String], col: String): Int = {
    @tailrec
    def helper(crtList: List[String], index: Int): Int =
      crtList match {
        case x :: xs => if (x == col) index else helper(xs, index + 1)
        case Nil => -1
      }
    helper(list, 0)
  }

  def selectColumn(col: String): Dataset =
    val colIndex = searchIndex(data.head, col)
    new Dataset(data.map(line => List(line(colIndex))))

  def selectColumns(cols: List[String]): Dataset =
    // lista cu indecsii coloanelor cautate
    val colsIndex = cols.map(col => searchIndex(data.head, col))
    new Dataset(data.map(line => colsIndex.map(index => line(index))))

  /*
  in train se pun mereu primele nr - 1 elemente din lista data ca parametru
  in test scapam de primele nr - 1 elemente pt ca sunt cele puse deja in train
  si il pastram doar pe al nr-lea
  pt fiecare pas de recursivitate scapam de elementele adaugate deja prin drop(nr)
  */
  private def helper_split(sortedData: List[List[String]], nr: Int): (List[List[String]], List[List[String]]) = {
    if (sortedData.isEmpty) {
      (List(), List())
    } else {
      val (train, test) = helper_split(sortedData.drop(nr), nr)
      (sortedData.take(nr - 1) ++ train, sortedData.drop(nr - 1).take(1) ++ test)
    }
  }

  // sortam si dupa impartim datele cu ajutorul functiei helper_split
  def split(percentage: Double): (Dataset, Dataset) =
    val nr = (1 / percentage).ceil.toInt
    val sorted = data.tail.sortBy(_.head)
    val (train, test) = helper_split(sorted, nr)
    (new Dataset(data.head :: train), new Dataset(data.head :: test))

  def size: Int = data.length
  def getRows: List[List[String]] = data.tail
  def getHeader: List[String] = data.head
}

object Dataset {
  def apply(csv_filename: String): Dataset =
    val source = Source.fromFile(csv_filename)
    val lines = source.getLines().toList
    val data = lines.map(_.split(',').toList)
    new Dataset(data)

  def apply(ds: List[List[String]]): Dataset =
    new Dataset(ds)
}
