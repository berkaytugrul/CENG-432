trait RandomStuffTrait {
  def transform(list: List[Int], op: (Int) => Int): List[Int]

  def allValid(list: List[Int], op: (Int) => Boolean): Boolean

  def executeWithRetry(retryCount: Int, op: => Int): Option[Int]

}

object RandomStuff extends RandomStuffTrait {
  def transform(list: List[Int], op: (Int) => Int): List[Int] = {
    for {
      i <- list
    } yield op(i)
  }

  def allValid(list: List[Int], op: (Int) => Boolean): Boolean = {
    var temp = list
    while (temp.nonEmpty) {
      if (!op(temp.head)) {
        return false
      }
      temp = temp.tail
    }
    true
  }

  def executeWithRetry(retryCount: Int, op: => Int): Option[Int] = {
    var count = retryCount
    if(retryCount < 0) {
      count = 0
    }
    while (count >= 0){
      try {
        return Option(op)
      } catch {
        case e: Exception => count = count - 1
      }
    }
    None
  }
}