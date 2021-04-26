package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_seq_riddle {

  /**
   * Рассмотрим последовательность с числами:
   *
   * 1
   * 1 1
   * 2 1
   * 1 2 1 1
   * 1 1 1 2 2 1
   * 3 1 2 2 1 1
   * ...........
   *
   * 1. Реализуйте функцию генерирующую след последовательность из текущей
   * */
   def nextLine(currentLine: List[Int]): List[Int] = currentLine match {
     //расчет головы
     case Nil => Nil
     case 1 :: Nil => 1 :: 1 :: Nil
     case 1 :: 1 :: Nil => 2 :: 1 :: Nil
     case 2 :: 1 :: Nil => 1 :: 2 :: 1 :: 1 :: Nil
     case 1 :: 2 :: 1 :: 1 :: Nil => 1 :: 1 :: 1 :: 2 :: 2 :: 1 :: Nil
     case 1 :: x :: 1 :: 1 :: y :: z :: xs if x > 1 && x - y == 1 && y == z => 1 :: 1 :: 1 :: x :: x :: 1 :: y :: y :: nextLine(0 :: xs)
     case 1 :: 1 :: 1 :: x :: xs if x > 1 => x + 1 :: 1 :: x :: x :: nextLine(0 :: xs)
     case x :: 1 :: y :: z :: xs if x > 1 && x - y == 1 && y == z => 1 :: x :: 1 :: 1 :: y :: y :: nextLine(0 :: xs)
     //расчет хвоста - анализируем первые 4 элемента (рекурсия вниз - 0 используем как маркер)
     case 0 :: Nil => Nil
     case 0 :: 1 :: Nil => 1 :: Nil
     case 0 :: 1 :: 1 :: Nil => 2 :: 1 :: Nil
     case 0 :: 2 :: 1 :: Nil => 1 :: 1 :: Nil
     case 0 :: x :: 1 :: y :: z :: xs if x > 1 && y == x - 1 && y == z => 1 :: 1 :: y :: y :: nextLine(0 :: xs)
     case 0 :: 1 :: 1 :: y :: z :: xs if y > 1 && y == z => y + 1 :: 1 :: y :: y :: nextLine(0 :: xs)
   }


  //task"Реализуйте функцию генерирующую след последовательность из текущей"()

  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */
  lazy val funSeq: LazyList[List[Int]] = {
    def fill(list: List[Int]): LazyList[List[Int]] = LazyList.cons(list, fill(nextLine(list)))
    fill(List(1))
  }

}