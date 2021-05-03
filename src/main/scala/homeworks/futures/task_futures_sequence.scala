package homeworks.futures

import homeworks.HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */

  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A:ClassTag](futures: List[Future[A]])
                              (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] =
      futures.foldRight(Future.successful((List.empty[A], List.empty[Throwable])))((f, coll) =>
      for {
        v <- f.recover { case e: Throwable => e }
        (values, errors) <- coll
      } yield v match {
        case v: A => (v :: values, errors)
        case e: Throwable => (values, e :: errors)
      })

}
