package Solutions

object Solution9 {
  def isSubstringPresent(s: String): Boolean = {
    s.indices.flatMap { case i if i != s.length - 1 => Some(s.substring(i, i + 2))
                        case other => None }.exists(s.reverse.indexOf(_) != -1)
  }
}
