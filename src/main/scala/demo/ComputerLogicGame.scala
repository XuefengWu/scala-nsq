package demo

import scala.collection.mutable.ListBuffer



trait BoxIndex  {
  def get(implicit boxes: ListBuffer[Int]): Int
}

case class DirectIndex(index: Int) extends BoxIndex {
  def get(implicit boxes: ListBuffer[Int]): Int = index
}

case class AddressIndex(index: Int) extends BoxIndex {
  def get(implicit boxes: ListBuffer[Int]): Int = boxes(index)
}

trait Value {
  def get(implicit boxes: ListBuffer[Int], steps: ListBuffer[Instruction]): Int
}
case class DirectValue(v: Int) extends Value {
  override def get(implicit boxes: ListBuffer[Int], steps: ListBuffer[Instruction]): Int = v
}

case class BoxValue(index: Int) extends Value {
  override def get(implicit boxes: ListBuffer[Int], steps: ListBuffer[Instruction]): Int = boxes(index)
}

case class OperationBoxValue(stepIndex: Int, boxOrder: OperationOrder.OperationOrder) extends Value {
  override def get(implicit boxes: ListBuffer[Int], steps: ListBuffer[Instruction]): Int = {
    val step = steps(stepIndex).asInstanceOf[Operation]
    boxOrder match {
      case OperationOrder.First => boxes(step.first.get)
      case OperationOrder.Second => boxes(step.second.get)
    }
  }
}

case class OperationBoxIndex(stepIndex: Int, boxOrder: OperationOrder.OperationOrder) extends Value {
  override def get(implicit boxes: ListBuffer[Int], steps: ListBuffer[Instruction]): Int = {
    val step = steps(stepIndex).asInstanceOf[Operation]
    boxOrder match {
      case OperationOrder.First => step.first.get
      case OperationOrder.Second => step.second.get
    }
  }
}

trait CalcFunction extends ((Int, Int) => Int)
case object AddFunction extends CalcFunction {
  def apply(a: Int, b: Int): Int = a + b
}
case object MultiplyFunction extends CalcFunction {
  def apply(a: Int, b: Int): Int = a + b
}
trait BooleanFunction extends ((Int, Int) => Boolean)
case object GreaterFunction extends BooleanFunction {
  def apply(a: Int, b: Int): Boolean = a > b
}

case class Context(var pc: Int)

trait Instruction {
  def execute(implicit buffer: ListBuffer[Int], steps: ListBuffer[Instruction], context: Context): Unit = {}
}
case object Begin extends Instruction
case object End extends Instruction

object OperationOrder extends Enumeration {
  type OperationOrder = Value
  val First, Second = Value
}

case class Operation(op: CalcFunction, first: BoxIndex, second: BoxIndex, dest: BoxIndex) extends Instruction {
  override def execute(implicit boxes: ListBuffer[Int], steps: ListBuffer[Instruction], context: Context): Unit = {
    val res = op(boxes(first.get), boxes(second.get))
    boxes(dest.get) = res
  }
}

case class Put(src: BoxIndex, dest: BoxIndex) extends Instruction {
  override def execute(implicit boxes: ListBuffer[Int], steps: ListBuffer[Instruction], context: Context): Unit = {
    boxes(dest.get) = boxes(src.get)
  }
}

case class IncreaseOperationBox(stepIndex: Int, boxOrder: OperationOrder.OperationOrder, value: Value) extends Instruction {
  override def execute(implicit boxes: ListBuffer[Int], steps: ListBuffer[Instruction], context: Context): Unit = {
    val step = steps(stepIndex).asInstanceOf[Operation]
    val _step = boxOrder match {
      case OperationOrder.First => step.copy(first = DirectIndex(step.first.get + value.get))
      case OperationOrder.Second => step.copy(second = DirectIndex(step.second.get + value.get))
    }
    steps(stepIndex) = _step
  }
}

case class If(cond: BooleanFunction, a: Value, b: Value, trueJump: Int, falseJump: Int) extends Instruction {
  override def execute(implicit boxes: ListBuffer[Int], steps: ListBuffer[Instruction], context: Context): Unit = {
    context.pc = if(cond(a.get,b.get)) {
      trueJump
    } else {
      falseJump
    }
    steps(context.pc).execute
  }
}

case class Jump(pc: Int, express: Instruction) extends Instruction {
  override def execute(implicit boxes: ListBuffer[Int], steps: ListBuffer[Instruction], context: Context): Unit = {
    express.execute
    context.pc = pc
  }
}

object ComputerLogicGame extends App {

  implicit val context = Context(0)
/*
  implicit val boxes = ListBuffer[Int](0,4,3,9,2,7,2,63,36,55,9)

  implicit val steps = List(
    Begin,
    Operation((a, b) => a + b, DirectIndex(4),DirectIndex(2),DirectIndex(8)),
    Operation((a, b) => a + b, DirectIndex(8),AddressIndex(6),DirectIndex(6)),
    Operation((a, b) => a * b, DirectIndex(6),DirectIndex(1),DirectIndex(4))
  )
  steps.foreach(_.execute)
  println(boxes(4))
*/

  implicit val boxes = ListBuffer(0,2,7,2,1,5,7,1,4)
  implicit val steps = ListBuffer[Instruction](
    Begin,
    Put(DirectIndex(7), DirectIndex(1)),
    Operation(AddFunction, DirectIndex(1),DirectIndex(2),DirectIndex(1)),
    IncreaseOperationBox(2, OperationOrder.Second, DirectValue(1)),
    If(GreaterFunction, OperationBoxIndex(2, OperationOrder.Second), BoxValue(8), 5, 2),
    End
  )

  while(context.pc < steps.length) {
    steps(context.pc).execute
    context.pc += 1
  }

  println(boxes(1))

}
