package chapter06

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = 
  State(initial =>
    val updated = inputs.foldLeft(initial){
      case (machine, _) if machine.candies == 0 => machine
      case (Machine(true, candies, coins), Input.Coin) => Machine(false, candies, coins + 1)
      case (Machine(false, candies, coins), Input.Turn) => Machine(true, candies - 1, coins)
      case (machine, _) => machine
    }

    ((updated.candies, updated.coins), updated)
  )
