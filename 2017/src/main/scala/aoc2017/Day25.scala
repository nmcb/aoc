package aoc2017

import nmcb.*
import nmcb.predef.*

import scala.collection.Iterator.iterate

object Day25 extends AoC:

  /**
   * Type aliases:
   */
  type State       = Char
  type Value       = Int
  type Move        = Int
  type Position    = Int
  type Tape        = Map[Position, Value]
  type Transitions = Map[(from: State, read: Value), (write: Value, move: Move, to: State)]

  /**
   * State Transition Machine:
   */
  val transitions: Transitions =
    Map(
      /**
       * In state A:
       *   If the current value is 0:
       *     - Write the value 1.
       *     - Move one slot to the right.
       *     - Continue with state B.
       *   If the current value is 1:
       *     - Write the value 0.
       *     - Move one slot to the left.
       *     - Continue with state D.
       */
      (from = 'A', read = 0) -> (write = 1, move = +1, to = 'B'),
      (from = 'A', read = 1) -> (write = 0, move = -1, to = 'D'),

      /**
       * In state B:
       *   If the current value is 0:
       *     - Write the value 1.
       *     - Move one slot to the right.
       *     - Continue with state C.
       *   If the current value is 1:
       *     - Write the value 0.
       *     - Move one slot to the right.
       *     - Continue with state F.
       */
      (from = 'B', read = 0) -> (write = 1, move = +1, to = 'C'),
      (from = 'B', read = 1) -> (write = 0, move = +1, to = 'F'),

      /**
       * In state C:
       *   If the current value is 0:
       *     - Write the value 1.
       *     - Move one slot to the left.
       *     - Continue with state C.
       *   If the current value is 1:
       *     - Write the value 1.
       *     - Move one slot to the left.
       *     - Continue with state A.
       */
      (from = 'C', read = 0) -> (write = 1, move = -1, to = 'C'),
      (from = 'C', read = 1) -> (write = 1, move = -1, to = 'A'),

      /**
       * In state D:
       *   If the current value is 0:
       *     - Write the value 0.
       *     - Move one slot to the left.
       *     - Continue with state E.
       *   If the current value is 1:
       *     - Write the value 1.
       *     - Move one slot to the right.
       *     - Continue with state A.
       */
      (from = 'D', read = 0) -> (write = 0, move = -1, to = 'E'),
      (from = 'D', read = 1) -> (write = 1, move = +1, to = 'A'),

      /**
       * In state E:
       *   If the current value is 0:
       *     - Write the value 1.
       *     - Move one slot to the left.
       *     - Continue with state A.
       *   If the current value is 1:
       *     - Write the value 0.
       *     - Move one slot to the right.
       *     - Continue with state B.
       */
      (from = 'E', read = 0) -> (write = 1, move = -1, to = 'A'),
      (from = 'E', read = 1) -> (write = 0, move = +1, to = 'B'),

      /**
       * In state F:
       *   If the current value is 0:
       *     - Write the value 0.
       *     - Move one slot to the right.
       *     - Continue with state C.
       *   If the current value is 1:
       *     - Write the value 0.
       *     - Move one slot to the right.
       *     - Continue with state E.
       */
      (from = 'F', read = 0) -> (write = 0, move = +1, to = 'C'),
      (from = 'F', read = 1) -> (write = 0, move = +1, to = 'E'),
    )

  /**
   * Turing Machine.
   **/
  case class Turing(
    transitions: Transitions,
    state: State,
    cursor: Position = 0,
    tape: Tape       = Map.empty.withDefaultValue(0)
  ):

    def step: Turing =
      val transition = transitions((from = state, read = tape(cursor)))
      copy(state = transition.to, cursor = cursor + transition.move, tape = tape.updated(cursor, transition.write))

  /**
   * Begin in state A.
   */
  val beginState: State = 'A'

  /**
   * Turing Machine Instance.
   */
  def turing: Turing = Turing(transitions, beginState)

  /**
   * Perform a diagnostic checksum after 12317297 steps.
   */
  override lazy val answer1: Int = iterate(turing)(_.step).nth(12317297).tape.count(_.right == 1)
