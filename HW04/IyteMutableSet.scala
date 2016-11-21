class IyteMutableSet
{
  private var aMutableSet = new Array[Int](0)
  private var aUnOrderedMutableSet = new Array[Int](0)

  def add(x: Int): IyteMutableSet =
  {
    val tmpImMutableSet = new IyteMutableSet()
    if  (contains(x) != true)
    {
      aMutableSet = addArrayWithOrder(aMutableSet,x)
      aUnOrderedMutableSet = addArrayAppend(aUnOrderedMutableSet,x);
    }
    return this
  }

  def addArrayAppend(aArray: Array[Int],IntX: Int): Array[Int] =
  {
    val newArr = new Array[Int](aArray.length + 1)
    for (i <- 0 to (aArray.length - 1))
      newArr(i) = aArray(i) // 1
    newArr(aArray.length) = IntX
    return newArr
  }

  def addArrayWithOrder(aArary: Array[Int],IntX: Int): Array[Int] =
  {
    val i = 0
    val j = 0
    var newArrItemIndex = 0
    val newArrLength = aArary.length + 1

    val newArr = new Array[Int](newArrLength)

    if (aArary.length == 0 )
    {
      newArr(newArrItemIndex) = IntX;
      newArrItemIndex = newArrItemIndex + 1;
    }
    else
    {

        for (i <- 0 to (aArary.length - 1))
        {
          if (aArary(i) < IntX) {
            newArr(newArrItemIndex) = aArary(i);
            newArrItemIndex = newArrItemIndex + 1;
            if (i == (aArary.length - 1)) {
              newArr(newArrItemIndex) = IntX;
              newArrItemIndex = newArrItemIndex + 1;
            }
          }
          else
          if (aArary(i) > IntX) {
            newArr(newArrItemIndex) = IntX;
            newArrItemIndex = newArrItemIndex + 1;
            for (j <- i to (aArary.length - 1)) {
              newArr(newArrItemIndex) = aArary(j);
              newArrItemIndex = newArrItemIndex + 1;
            }
            return newArr;
          }
          else
          if (aArary(i) == IntX) {
            newArr(newArrItemIndex) = aArary(i);
            newArrItemIndex = newArrItemIndex + 1;
            for (j <- i + 1 to (aArary.length - 1)) {
              newArr(newArrItemIndex) = aArary(j);
              newArrItemIndex = newArrItemIndex + 1;
            }
            return newArr;
          }
        }

    }
    aMutableSet = newArr
    return aMutableSet
  }

  def contains(x: Int): Boolean =
  {
    return searchElement(aMutableSet,x) > -1
  }

  def searchElement(aArary: Array[Int], Element: Int): Int =
  {
    var LeftIndex = 0
    var RightIndex = aArary.length-1
    while (LeftIndex<=RightIndex)
    {
      val MiddleIndex = LeftIndex + (RightIndex - LeftIndex) / 2
      if (aArary(MiddleIndex) == Element)
        return MiddleIndex
      else
      if (aArary(MiddleIndex) > Element)
        RightIndex = MiddleIndex-1
      else
        LeftIndex = MiddleIndex+1
    }
    return -1
  }

  private def toStringL: String =
  {
    val resultStr = "";
    println(aUnOrderedMutableSet.mkString(","))
    return resultStr
  }

  override def toString(): String = {
    toStringL
    //    aUnOrderedMutableSet.mkString(",")
  }
}

object IyteMutableSet
{
  def apply() = new IyteMutableSet()
}