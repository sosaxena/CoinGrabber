package coinGrabber

object CoinGrabber {
  
 
   def main(args: Array[String]): Unit = {
    var board = Array(
    Array(0,5, 10, 5,25),
    Array(5, 10, 25,10,5),
    Array(10, 25, 1,25,10),
    Array(5,10,25,10,5),
    Array(25,5,10,5,0))
   /**
    * You can change this heuristic constant to whichever depth you like.
    */
    val heuristicDepth=13
    var initialState=State(board,(0,0),0,(4,4),0)
    
    println("\n *************************** Welcome to Coin Grabber ************************\n ")
    println("\n H is you and C is the computer. \n This is your board :  \n")
    printBoard(initialState);
     
     var input=getUserInput()
     var flag=0
     while(!((initialState.locationHuman._1==2 && initialState.locationHuman._2==2) || (initialState.locationComputer._1==2 && initialState.locationComputer._2==2))){
        if(input==0){
          var move=chooseMove(heuristicDepth,initialState)
          var newState=movePlayer('c',move,initialState)
           printBoard(newState)
          initialState=newState
          input=1
        }
        else if(input==1){
          var move=getHumanInput(initialState)
          var newState=movePlayer('h',move,initialState)
          printBoard(newState)
          initialState=newState
          input=0
         }
     }
     if(initialState.locationHuman._1==2 && initialState.locationHuman._2==2 ){
         if(initialState.humanMoney>initialState.computerMoney && initialState.humanMoney>100) println("\n You Win !! \n Your money : " + initialState.humanMoney + " and Computer's money : "+ initialState.computerMoney)
         else println("\n Computer Wins, you lose !! \n Your money : " + initialState.humanMoney + " and Computer's money : "+ initialState.computerMoney)
     }
     if(initialState.locationComputer._1==2 && initialState.locationComputer._2==2 && initialState.computerMoney>100 ){
       if(initialState.humanMoney<initialState.computerMoney) println("\n Computer Wins, you lose !! \n Your money : " + initialState.humanMoney + " and Computer's money : "+ initialState.computerMoney)
       else println("\n You Win !! \n Your money : " + initialState.humanMoney + " and Computer's money : "+ initialState.computerMoney)
     }
   }
  
    
  /**
   * Choose who plays first; the human or the computer.
   */
  def getUserInput():Int={
     
    println("\n Who gets to play first ? \n Type 1 for human or 0 for computer \n");
    var input: Int = Console.readInt 
    if(input==1|| input==0){
       return input
    }
    else{
      return getUserInput()
    }
  }
   
  /**
   * Remove a value from a given list.
   */
   def remove(d: Char, list: List[Char]) = list diff List(d)
    
   /**
    * Given a player, 'h' or 'c', and a State, this method returns a list of  ('U', 'D', 'L', or 'R') that can be applied.
    */
   def possibleMoves(player: Char, state: State):List[Char]={
    
    if(player=='h'){
      var humanLocation=state.locationHuman
      var x= humanLocation._1
      var y=humanLocation._2
      var humanMoney=state.humanMoney
      var list=List('D','R','L','U')
      if(x+1>=5 || (state.locationComputer._1==x+1 && state.locationComputer._2==y)){
        list = remove('D',list)
      }
      if(x-1<0 || (state.locationComputer._1==x-1 && state.locationComputer._2==y)){
        list=remove('U',list)
      }
      if(y+1>=5 || (state.locationComputer._1==x && state.locationComputer._2==y+1)){
        list=remove('R',list)
      }
      if(y-1<0 || (state.locationComputer._1==x && state.locationComputer._2==y-1)){
        list=remove('L',list)
      }
      return list
    }
    else if(player=='c'){
      var computerLocation=state.locationComputer
      var x= computerLocation._1
      var y=computerLocation._2
      var computerMoney=state.computerMoney
      var list=List('D','R','L','U')
      if(x+1>=5 || (state.locationHuman._1==x+1 && state.locationHuman._2==y)){
        list=remove('D',list)
      }
      if(x-1<0 || (state.locationHuman._1==x-1 && state.locationHuman._2==y)){
        list=remove('U',list)
      }
      if(y+1>=5 || (state.locationHuman._1==x && state.locationHuman._2==y+1)){
        list=remove('R',list)
      }
      if(y-1<0 || (state.locationHuman._1==x && state.locationHuman._2==y-1)){
        list=remove('L',list)
      }
      return list
    }
    else{
      return List()
    }
  }
   
   
   /**
    * This method returns the new State that results from moving the player in the given direction.
    */
   def nextState(player: Char, direction: Char, state: State): State={
      if(player=='c'){
        var x=state.locationComputer._1
        var y=state.locationComputer._2
        var money=state.computerMoney
        if(direction=='D'){
          var p=x+1
          money=money+state.board(x+1)(y)
          var copyBoard = state.board.map(_.clone) //The original state is not changed.
          copyBoard(x + 1)(y) = 0
          return State(copyBoard,state.locationHuman,state.humanMoney,(p,y),money)
         }
        if(direction=='U'){
          var p=x-1
          money=money+state.board(x-1)(y)
          var copyBoard = state.board.map(_.clone)
          copyBoard(x - 1)(y) = 0
          return State(copyBoard,state.locationHuman,state.humanMoney,(p,y),money)
         }
         if(direction=='L'){
           var p=y-1
           money=money+state.board(x)(y-1)
           var copyBoard = state.board.map(_.clone)
           copyBoard(x)(y-1) = 0
           return State(copyBoard,state.locationHuman,state.humanMoney,(x,p),money)
         }
         if(direction=='R'){
           var p=y+1
           money=money+state.board(x)(y+1)
           var copyBoard = state.board.map(_.clone)
           copyBoard(x)(y+1) = 0
           return State(copyBoard,state.locationHuman,state.humanMoney,(x,p),money)
         }
       }
     if(player=='h'){
      var x=state.locationHuman._1
      var y=state.locationHuman._2
      var money=state.humanMoney
      if(direction=='D'){
         var p=x+1
         money=money+state.board(x+1)(y)
         var copyBoard = state.board.map(_.clone)
         copyBoard(x + 1)(y) = 0
         return State(copyBoard,(p,y),money,state.locationComputer,state.computerMoney)
       }
      if(direction=='U'){
        var p=x-1
        money=money+state.board(x-1)(y)
        var copyBoard = state.board.map(_.clone)
        copyBoard(x - 1)(y) = 0
        return State(copyBoard,(p,y),money,state.locationComputer,state.computerMoney)
      }
      if(direction=='L'){
        var p=y-1
        money=money+state.board(x)(y-1)
        var copyBoard = state.board.map(_.clone)
        copyBoard(x)(y-1) = 0
        return State(copyBoard,(x,p),money,state.locationComputer,state.computerMoney)
      }
      if(direction=='R'){
        var p=y+1
        money=money+state.board(x)(y+1)
        var copyBoard = state.board.map(_.clone)
        copyBoard(x)(y+1) = 0
        return State(copyBoard,(x,p),money,state.locationComputer,state.computerMoney)
      }
    }
     return state
   }
  
   
   /**
    * This method chooses the next move for the computer.
    */
   
   def chooseMove(depth: Int, state: State): Char={
     
     var l=possibleMoves('c',state)
     // create an empty map
     var moves = scala.collection.mutable.Map[Int, Char]()
     for(c <- l){
       var e=evaluateMove('c',c,depth,state)
       moves+=(e-> c)
     }
     var max:Int=0
     for(entry <- moves){
        if(entry._1>max){
          max=entry._1
        }
     }
     return moves(max)
     
   }
   
  
   /**
    * This method returns the value of the move (larger values are better) for the given player.
      If the depth is zero, the value returned is the value of the heuristic function.
      If the move is into the center of the board, thus ending the game, the value returned is the value of the heuristic function.
      Otherwise, the move is made. Then, for each of the other player's possible moves, those moves are evaluated (recursively, with depth - 1), and the largest value is chosen.
      The value returned is the negative of the value found in the previous step.
    */
  def evaluateMove(player: Char, direction: Char, depth: Int, state: State): Int={
     if(depth==0) return heuristic(player,state)
     
     if(player=='c'){
       if(state.locationComputer._1==2 && state.locationComputer._2==2){
         return heuristic(player,state)
       }
       else{
         var newState =nextState('c',direction,state) //Make the move if not in the center
         //println("Going up to a new state")
         var l=possibleMoves('h',newState)
         var results = l.map(x => evaluateMove('h', x, depth -1, newState))
         var max:Int=0
         for( m:Int <- results){
           if(m>max) max=m
         }
         return -1*max 
       }
      }
     else if(player=='h'){
       if(state.locationHuman._1==2 && state.locationHuman._2==2){
         return heuristic(player,state)
        }
       else{
         var newState =nextState(player,direction,state) //Make the move if not in the center
         var l=possibleMoves('c',newState)
         var results = l.map(x => evaluateMove('c', x, depth -1, newState))
         var max:Int=0
         for( m:Int <- results){
           if(m>max) max=m
         }
          return -1*max
        }
      }
     else{
      return 0
     }
   }
   
   /**
    * Finds the value of the given state for the given player. The value should be very high if the player is in the center of the board with at least 100¢ 
    * and more money than the other player, and very low if the situation is reversed.
    * If the player is not at the enter, then just return the difference in money of the given player and opposite player.
    */
   def heuristic(player: Char, state: State): Int={
     if(player=='c'){
        if(state.locationComputer._1==2 && state.locationComputer._2==2){ //In center
          if(state.computerMoney<100){ //Less than 100 cents
            return -10000 //LOSING SITUATION
          }
          else{//More than 100 cents
            if(state.computerMoney>state.humanMoney){ //More than opponent
              return 10000 //WINNING SITUATION
            }
            else{ //Less than opponent
              return -10000 //LOSING SITUATION
            }
          }
        }
        else{ //Not yet ended
          return state.computerMoney-state.humanMoney
        }
     }
     if(player=='h'){
        if(state.locationHuman._1==2 && state.locationHuman._2==2){ //In center
          if(state.humanMoney<100){//Less than 100 cents
            return -10000
          }
          else{ //More than 100 cents
            if(state.humanMoney>state.computerMoney){
              return 10000
            }
            else{
              return -10000
            }
          }
        }
        else{
          return state.humanMoney-state.computerMoney
        }
     }
     return 0
   }
   
   /**
    * Prints the board to represent the money and positions on it at a given state.
    */
   
   def printBoard(state:State){
    println(" \n ")
    for(i <- 0 to state.board.length-1){
      for(j<- 0 to state.board(0).length-1){
         if(i==state.locationHuman._1 && j==state.locationHuman._2){
           print('H' + "  ")
         }
         else if(i==state.locationComputer._1 && j==state.locationComputer._2){
           print('C' + "  ")
         }
         else{
         print(state.board(i)(j) + "  ")
         }
        }
       println(" ")
      }
    }
   
   
   /**
    * Ask the user/human to enter word to move their player up, down ,left or right.
    * If move entered is not legal, ask again till user enters legal move.  
    */
   def getHumanInput(state:State):Char={
     var input=readLine("\n Enter your move: \n")
     if(input.startsWith("D") || input.startsWith("d")){
       if(!checkLegalMove(state,input)) getHumanInput(state)
       else return 'D'
     }
     else if(input.startsWith("U") || input.startsWith("u")){
       if(!checkLegalMove(state,input)) getHumanInput(state)
       else return 'U'
     }
     else if(input.startsWith("L") || input.startsWith("l")){
       if(!checkLegalMove(state,input)) getHumanInput(state)
       else return 'L'
     }
     else if(input.startsWith("R") || input.startsWith("r")){
       if(!checkLegalMove(state,input)) getHumanInput(state)
       else return 'R'
     }
     else{
       return getHumanInput(state)
     }
   }
   
   /**
    * Check if the string entered by the user is legal or not.
    * Return false if making the move will exceed board boundaries or make the given player land 
    * in a spot where the opposite player is.
    * Otherwise returns true.
    */
    def checkLegalMove(state:State,direction:String):Boolean={
      
      if(direction.startsWith("D") || direction.startsWith("d")){
        if(state.locationHuman._1+1>=5 || (state.locationComputer._1==state.locationHuman._1+1 && state.locationComputer._2==state.locationHuman._2)) return false
        else return true
      }
      if(direction.startsWith("U") || direction.startsWith("u")){
        if(state.locationHuman._1-1<0 || (state.locationComputer._1==state.locationHuman._1-1 && state.locationComputer._2==state.locationHuman._2)) return false
        else return true
      }
      if(direction.startsWith("L") || direction.startsWith("l")){
        if(state.locationHuman._2-1<0 || (state.locationComputer._1==state.locationHuman._1 && state.locationComputer._2==state.locationHuman._2-1)) return false
        else return true
      }
      if(direction.startsWith("R") || direction.startsWith("r")){
        if(state.locationHuman._2+1>=5 || (state.locationComputer._1==state.locationHuman._1 && state.locationComputer._2==state.locationHuman._2+1)) return false
        else return true
      }
      return false;
    }
    
   /**
    * Actually move the player in the given direction.
    * Change the board accordingly as well.
    */
   def movePlayer(player:Char,direction:Char,state:State):State={
     if(player=='c'){
        var x=state.locationComputer._1
        var y=state.locationComputer._2
        var money=state.computerMoney
        if(direction=='D'){
          var p=x+1
          money=money+state.board(x+1)(y)
          state.board(x + 1)(y) = 0
          println("\n Computer moved down \n")
          return State(state.board,state.locationHuman,state.humanMoney,(p,y),money)
         }
        if(direction=='U'){
          var p=x-1
          money=money+state.board(x-1)(y)
          state.board(x - 1)(y) = 0
           println("\n Computer moved up \n")
          return State(state.board,state.locationHuman,state.humanMoney,(p,y),money)
         }
         if(direction=='L'){
           var p=y-1
           money=money+state.board(x)(y-1)
           state.board(x)(y-1) = 0
            println("\n Computer moved left \n")
           return State(state.board,state.locationHuman,state.humanMoney,(x,p),money)
         }
         if(direction=='R'){
           var p=y+1
           money=money+state.board(x)(y+1)
           state.board(x)(y+1) = 0
            println("\n Computer moved right \n")
           return State(state.board,state.locationHuman,state.humanMoney,(x,p),money)
         }
       }
     if(player=='h'){
      var x=state.locationHuman._1
      var y=state.locationHuman._2
      var money=state.humanMoney
      if(direction=='D'){
         var p=x+1
         money=money+state.board(x+1)(y)
         state.board(x + 1)(y) = 0
         return State(state.board,(p,y),money,state.locationComputer,state.computerMoney)
       }
      if(direction=='U'){
        var p=x-1
        money=money+state.board(x-1)(y)
        state.board(x - 1)(y) = 0
        return State(state.board,(p,y),money,state.locationComputer,state.computerMoney)
      }
      if(direction=='L'){
        var p=y-1
        println(state.board(x)(y-1))
        money=money+state.board(x)(y-1)
        state.board(x)(y-1) = 0
        return State(state.board,(x,p),money,state.locationComputer,state.computerMoney)
      }
      if(direction=='R'){
        var p=y+1
         println(state.board(x)(y+1))
        money=money+state.board(x)(y+1)
        state.board(x)(y+1) = 0
        return State(state.board,(x,p),money,state.locationComputer,state.computerMoney)
      }
    }
     return state
     
   }
}