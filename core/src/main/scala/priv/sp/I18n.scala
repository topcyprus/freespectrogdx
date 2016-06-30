package priv.sp

import java.util.Scanner

import scala.io.{Codec, Source}

object I18n {
  val locale = Option(System.getenv("LC_CTYPE")) getOrElse java.util.Locale.getDefault().toString()
  val isRussian = locale.startsWith("ru_")
  val localeIndex = if (isRussian) 1 else 0
  val s = Source.fromURL(this.getClass.getClassLoader.getResource("i18n.csv"))(Codec.UTF8)
  val messages = s.getLines().collect { case l if l.nonEmpty =>
    val name :: values = l.split(";").toList
    name -> values
  }.toMap

  def apply(name : String) = {
    messages.get(name) match {
      case Some(values) =>
        if (values.size <= localeIndex) {
          values(0)
        } else {
          values(localeIndex)
        }
      case None => name
    }
  }

  def default(name : String) = {
    messages.get(name) match {
      case Some(values) => values(0)
      case None => name
    }
  }
}
