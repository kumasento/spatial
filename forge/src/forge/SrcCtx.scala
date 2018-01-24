package forge

import scala.reflect.macros.blackbox
import scala.language.experimental.macros

case class SrcCtx(dir: String, file: String, line: Int, column: Int, content: Option[String]) {
  override def toString = s"$file:$line:$column"
}
object SrcCtx {
  lazy val empty = SrcCtx("?", "?", 0, 0, None)

  implicit def _sc: SrcCtx = macro SourceContextMacro.impl
}


// Using the available information from the macro Context, rebuild the SourceContext previously generated by the compiler
private object SourceContextMacro {
  def impl(c: blackbox.Context): c.Expr[SrcCtx] = {
    import c.universe._
    val pos = c.enclosingPosition
    val path = pos.source.path
    val filename = pos.source.file.name
    val line = pos.line
    val column = pos.column
    val lineContent = if (line > 0) Some(pos.source.lineToString(line-1)) else None

    c.Expr(q"forge.SrcCtx($path, $filename, $line, $column, $lineContent)")
  }
}

