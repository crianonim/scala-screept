package site.jans.screept
import scala.collection.mutable

object StringOperators {
  def toValue(
      list: Seq[String],
      ctx: mutable.Map[String, String]
  ): Seq[String] = {
    list map (Screept.getValue(_, ctx))
  }
  def mathOperation(
      list: Seq[String],
      operation: Function2[Double, Double, Double]
  ): Double = {
    list.map(_.toDouble).reduce(operation)
  }

  def helper(
      list: Seq[String],
      ctx: mutable.Map[String, String],
      operation: Function2[String, String, String]
  ): String = {
    toValue(list, ctx) reduce operation
  }

  def equalityHelper(
      list: Seq[String],
      ctx: mutable.Map[String, String],
      operation: Function2[Double, Double, Boolean]
  ): String = {
    val values = toValue(list, ctx).map(_.toDouble)
    if (operation(values(0), values(1))) "1" else "0"
  }
  val operators = Map(
    "CONCAT" -> Operator(2, (l, ctx) => helper(l,ctx,(_+_))),
    "CONCAT3" -> Operator(3, (l, ctx) => helper(l,ctx,(_+_))),
    )

}
