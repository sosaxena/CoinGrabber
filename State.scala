package coinGrabber

case class State(var board: Array[Array[Int]], var locationHuman:(Int,Int),var humanMoney: Int, var locationComputer:(Int,Int),var computerMoney:Int){
  
}