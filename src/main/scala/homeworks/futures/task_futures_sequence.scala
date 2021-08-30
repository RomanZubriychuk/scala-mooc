package homeworks.futures

import homeworks.HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future}

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
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    futures match {
      case ::(head, next) => head.flatMap(success => fullSequence(next).map(tuple => (success :: tuple._1, tuple._2)))
        .recoverWith(failure => fullSequence(next).map(tuple => (tuple._1, failure :: tuple._2)))
      case Nil => Future.successful((List[A](), List[Throwable]()))
    }
  }
}
