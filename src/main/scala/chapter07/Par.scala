package chapter07

import java.util.concurrent.ExecutorService
import java.util.concurrent.TimeUnit
import java.util.concurrent.Future
import java.util.concurrent.Callable

opaque type Par[A] = ExecutorService => Future[A]

object Par:
  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A]:
    def isDone = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false

  extension [A](pa: Par[A])
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) =>
        val futureA = pa(es)
        val futureB = pb(es)
        new Future[C] {
          def isDone = true
          def get(): C = f(futureA.get, futureB.get)
          def get(timeout: Long, units: TimeUnit): C = f(
            futureA.get(timeout, units), futureB.get(timeout, units)
          )
          def isCancelled = false
          def cancel(evenIfRunning: Boolean): Boolean = false
        }
        

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] { def call = a(es).get })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = f andThen lazyUnit
