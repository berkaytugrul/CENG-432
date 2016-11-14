case class ImmutableList(head: Int, tail: IyteImmutableList) extends IyteImmutableList{
	override def add(x : Int) : IyteImmutableList =  ImmutableList(x,this)
	override def toString : String= {
		
		def getStr(str : IyteImmutableList) : String = str match {
			case Nul => Nul.toString
			case ImmutableList(x,xs) => x.toString + "," + getStr(xs)
		}
		
		val string = getStr(this)
		string.substring(0,string.length-1)
	}
}

case object Nul extends IyteImmutableList{
	override def add(x : Int) : IyteImmutableList = ImmutableList(x,Nul)
	override def toString = ""
}

sealed abstract class IyteImmutableList{
	def add(x : Int) :IyteImmutableList
}

object IyteImmutableList{
	def apply() = Nul
}