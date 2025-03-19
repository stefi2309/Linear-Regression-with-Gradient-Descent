import Dataset.*
import Matrix.*

import scala.annotation.tailrec

object Regression {

  def regression(dataset_file: String,
                 attribute_columns: List[String],
                 value_column: String,
                 test_percentage: Double,
                 alpha: Double,
                 gradient_descent_steps: Int): (Matrix, Double) = {
    // dataset ul de interes
    val dataset = Dataset(dataset_file).selectColumns(value_column :: attribute_columns)
    val (train, test) = dataset.split(test_percentage)
    // matrice de dim m * (n + 1)
    val X = Matrix(train.selectColumns(attribute_columns)) ++ 1.0
    // vector de coeficienti de dim (n + 1) * 1
    val W = Matrix(zeroList(X.width.getOrElse(0)).map(List(_)))
    // vector pret real de dim m * 1
    val Y = Matrix(train.selectColumn(value_column))
    // coef este vectorul de coeficienti curent
    @tailrec
    def gradient_descent(steps : Int, coef: Matrix): Matrix =
      if(steps == 0) coef
      else {
        // vector de estimari de dim m * 1
        val estimated_price = X * coef
        // vector pentru eroare de dim m * 1
        val error = estimated_price - Y
        // vector pentru gradient de dim (n + 1) * 1
        val gradient = (X.transpose * error).map(_ / X.height.getOrElse(1))
        //vectorul actualizat al coeficientilor
        val updated_W = coef - gradient.map(_ * alpha)
        gradient_descent(steps - 1, updated_W)
      }
    // vectorul final al estimarilor
    val final_W = gradient_descent(gradient_descent_steps, W)
    // eroarea finala de tip Double calculata prin media absoluta a diferentelor (MAE)
    val final_error = (X * final_W - Y).data match {
      case Some(matrix) =>
        matrix.foldLeft(0.0) { (acc, line) =>
          acc + line.foldLeft(0.0) { (lineAcc, elem) =>
            lineAcc + (if (elem < 0) -elem else elem)
          }
        } / Y.height.getOrElse(1)
      case None =>
        0.0
    }
    (final_W, final_error)
  }
  // functie pentru construirea unei liste de zerouri
  private def zeroList(size: Int): List[Double] =
    if(size <= 0) Nil
    else 0.0 :: zeroList(size - 1)

  def main(args: Array[String]): Unit = {
    // Exemplu de utilizare
    print(regression("datasets/houseds.csv", List("GrLivArea", "YearBuilt"), "SalePrice", 0.1, 1e-7, 10000))
  }
}