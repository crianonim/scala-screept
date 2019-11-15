package site.jans.screept
import scala.collection.mutable

object ObjectOperators {
  def toValue(
      list: Seq[String],
      ctx: mutable.Map[String, String]
  ): Seq[String] = {
    list map (Screept.getValue(_, ctx))
  }

  def helper(
      list: Seq[String],
      ctx: mutable.Map[String, String],
      operation: Function2[String, String, String]
  ): String = {
    toValue(list, ctx) reduce operation
  }

  val operators = Map(
    "OBJ" -> Operator(2, (l, ctx) => helper(l, ctx, (_ + "_" + _))),
    "VALZERO" -> Operator(
      1,
      (l, ctx) =>
        toValue(l, ctx)(0) match {
          case x if x == l(0) => "0.0"
          case x              => x
        }
    ),
    "ZERO" -> Operator(1, (l, ctx) => {
      ctx(l(0)) = "0"; "0"
    }),
    "ONE" -> Operator(1, (l, ctx) => {
      ctx(l(0)) = "1"; "1"
    })
  )

}
