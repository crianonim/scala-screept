package site.jans.screept
import scala.collection.mutable

object StringOperators {
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
    "CONCAT" -> Operator(2, (l, ctx) => helper(l,ctx,(_+_))),
    "CONCAT3" -> Operator(3, (l, ctx) => helper(l,ctx,(_+_))),
    )

}
