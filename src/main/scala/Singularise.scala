package hmrc.smartstub

/**
  * Based off of http://api.rubyonrails.org/classes/ActiveSupport/Inflector.html
  */
object Singularise{

  def apply(in: String): String = in match {
    case Irregular(i) => i
    case Regular(pattern, replacement) => pattern.replaceFirstIn(in, replacement)
    case x => x
  }

  object Regular {
    val rules = Seq(
      ("(quiz)zes$", "$1"),
      ("(matr|append)ices$", "$1ix"),
      ("(vert|ind)ices$", "$1ex"),
      ("^(ox)en", "$1"),
      ("(alias|status)$", "$1"), // already singular, but ends in 's'
      ("(alias|status)es$", "$1"),
      ("(octop|vir)us$", "$1us"), // already singular, but ends in 's'
      ("(octop|vir|cact)i$", "$1us"),
      ("(cris|ax|test)es$", "$1is"),
      ("(cris|ax|test)is$", "$1is"), // already singular, but ends in 's'
      ("(shoe)s$", "$1"),
      ("(o)es$", "$1"),
      ("(bus)es$", "$1"),
      ("([m|l])ice$", "$1ouse"),
      ("(x|ch|ss|sh)es$", "$1"),
      ("(m)ovies$", "$1ovie"),
      ("(s)eries$", "$1eries"),
      ("([^aeiouy]|qu)ies$", "$1y"),
      ("([lr])ves$", "$1f"),
      ("(tive)s$", "$1"),
      ("(hive)s$", "$1"),
      ("([^f])ves$", "$1fe"),
      ("(^analy)sis$", "$1sis"), // already singular, but ends in 's'
      ("(^analy)ses$", "$1sis"),
      ("((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)ses$", "$1$2sis"),
      ("([ti])a$", "$1um"),
      ("(n)ews$", "$1ews"),
      ("(s|si|u)s$", "$1s"), // '-us' and '-ss' are already singular
      ("tuses$", "tus"),
      ("(end|ul)a$", "$1um"),      
      ("ae$", "a"),
      ("s$", "")
    ).map{ case (a,b) => (a.r,b) }
    
    def unapply(in: String) = 
      rules.find(_._1.findFirstIn(in).isDefined)
  }

  object Irregular {

    val symmetric = "equipment,information,rice,money,species,series,fish,sheep".split(",")

    val all = Map (
      "person" -> "people",
      "man" -> "men",
      "child" -> "children",
      "sex" -> "sexes",
      "move" -> "moves",
      "stadium" -> "stadiums"
    )

    def unapply(i: String): Option[String] = all.get(i).orElse {
      if (symmetric.contains(i)) Some(i) else None
    }
  }

}
