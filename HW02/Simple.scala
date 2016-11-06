object Simple extends App {
	val array = Array(10,25,30)
	array.foreach(e=> if(e%2==0) println(e*2) else println(e*3))	
}