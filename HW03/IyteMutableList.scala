class IyteMutableList(){
	case class Node(value:Int,var next : Node)
	private var first:Node = null
	private var last:Node = first
	

	def add(x :Int):Unit={
		if(first == null){
			first=createNode(x)
			last=first
		}else{
			last.next=createNode(x)
			last=last.next
		}
	}
	override def toString() : String = {
		var temp = first
		var string = ""
		while(temp != null){
			string += temp.value + ","
			temp=temp.next
		}
		string.substring(0,string.length-1)
	}
	private def createNode(x :Int) : Node = Node(x,null)
}

object IyteMutableList{
	def apply() = new IyteMutableList()
}