class IyteImmutableSet
{
  private var newArrLength = 0;
  private var newArrIndexOfItem = -1;
  private var aImMutableSet = new Array[Int](0)
  private var aUnOrderedImMutableSet = new Array[Int](newArrLength)


  def add(x: Int): IyteImmutableSet =
  {
    val tmpImMutableSet = new IyteImmutableSet()
    if  (contains(x) != true)
    {
      tmpImMutableSet.aImMutableSet = addArrayWithOrder(aImMutableSet,x)
      tmpImMutableSet.aUnOrderedImMutableSet = addArrayAppend(aUnOrderedImMutableSet,x)
      tmpImMutableSet.newArrIndexOfItem = newArrIndexOfItem;
      return tmpImMutableSet
    }
    else
      return this
  }

  def addArrayAppend(aArray: Array[Int],IntX: Int): Array[Int] =
  {
      newArrLength = aArray.length + 1
      val newArr = new Array[Int](newArrLength)
      for (i <- 0 to (aArray.length - 1))
        newArr(i) = aArray(i) // 1
      newArr(aArray.length) = IntX
      newArrIndexOfItem = aArray.length
      return newArr
  }

  def addArrayWithOrder(aArray: Array[Int],IntX: Int): Array[Int] =
  {
    var newArrItemIndex = 0
    val newArrLength = aArray.length + 1
    val newArr = new Array[Int](newArrLength)
    if (aArray.length == 0 )
    {
      newArr(newArrItemIndex) = IntX;
      newArrItemIndex = newArrItemIndex + 1;
    }
    else
    {
        for (i <- 0 to (aArray.length - 1))
        {
          if (aArray(i) < IntX)
          {
            newArr(newArrItemIndex) = aArray(i);
            newArrItemIndex = newArrItemIndex + 1;
            if (i == (aArray.length - 1))
            {
              newArr(newArrItemIndex) = IntX;
              newArrItemIndex = newArrItemIndex + 1;
            }
          }
          else
          if (aArray(i) > IntX)
          {
            newArr(newArrItemIndex) = IntX;
            newArrItemIndex = newArrItemIndex + 1;
            for (j <- i to (aArray.length - 1))
            {
              newArr(newArrItemIndex) = aArray(j);
              newArrItemIndex = newArrItemIndex + 1;
            }
            return newArr
          }
          else
          if (aArray(i) == IntX)
          {
            newArr(newArrItemIndex) = aArray(i);
            newArrItemIndex = newArrItemIndex + 1;
            for (j <- i + 1 to (aArray.length - 1))
            {
              newArr(newArrItemIndex) = aArray(j);
              newArrItemIndex = newArrItemIndex + 1;
            }
            return newArr
          }
        }
    }
//    aImMutableSet = newArr
    return newArr
  }

  def contains(x: Int): Boolean =
  {
    return searchElement(aImMutableSet,x) > -1
  }

  def searchElement(aArray: Array[Int], Element: Int): Int =
  {
    var LeftIndex = 0
    var RightIndex = aArray.length-1
    while (LeftIndex<=RightIndex)
    {
      val MiddleIndex = LeftIndex + (RightIndex - LeftIndex) / 2
      if (aArray(MiddleIndex) == Element)
        return MiddleIndex
      else
      if (aArray(MiddleIndex) > Element)
        RightIndex = MiddleIndex-1
      else
        LeftIndex = MiddleIndex+1
    }
    return -1
  }

  private def toStringL: String =
  {
    val resultStr = "";
    println(aUnOrderedImMutableSet.mkString(","))
    return resultStr
  }

  override def toString(): String = {
    toStringL
//    aUnOrderedImMutableSet.mkString(",")
  }
}

object IyteImmutableSet
{
  def apply() = new IyteImmutableSet()
}