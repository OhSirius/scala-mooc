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
   def nextLine(currentLine: List[Int]): List[Int] = currentLine.foldRight(List.empty[Int])((v, cur) => (v, cur) match {
       case (1, Nil) => 1 :: 1 :: Nil
       case (1, 1 :: 1 :: Nil) => 2 :: 1 :: Nil
       case (2, 1 :: 1 :: Nil) => 1 :: 2 :: 1 :: 1 :: Nil
       case (2, 2 :: 1 :: Nil) => 2 :: 2 :: 1 :: Nil
       case (1, 2 :: 2 :: 1 :: Nil) => 1 :: 1 :: 1 :: 2 :: 2 :: 1 :: Nil
       case (1, 2 :: 2 :: 1 :: Nil) => 1 :: 2 :: 2 :: 1 :: Nil

       case (x, 1 :: y :: 1 :: 1 :: xs) if x==y => x :: 1 :: 1 :: xs
       case (1, x :: 1 :: 1 :: xs) => x :: x :: 1 :: 1 :: xs
       case (1, x :: y :: 1 :: 1 :: xs) if x==y => 1 :: x :: x :: 1 :: 1 :: xs
       case (1, 1 :: x :: y :: 1 :: xs) if x==y => x + 1 :: 1 :: x :: x :: 1 :: xs
       case (x, y :: z :: 1 :: xs) if x==y && y==z => x :: x :: x :: 1 :: xs
       case (1, x :: y :: z :: 1 :: xs) if x==y && y==z  => 1 :: x :: x :: x :: 1 :: xs
       case (x, 1 :: y :: z :: u :: xs) if x-y==1 && y==z && z == u => 1 :: x :: 1 :: 1 :: y :: y :: y :: xs
       case (x, y :: 1 :: 1 :: xs) if x==y => x :: x :: 1 :: 1 :: xs
       case (x, y :: 1 :: z :: u :: xs) if x==y && x-z==1 && x-u ==1 => x :: x :: 1 :: z :: z :: xs
       case (1, x :: y :: 1 :: z :: xs) if x==y && x-z == 1 => 1 :: 1 :: 1 :: x :: x :: 1 :: z :: xs
     })

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