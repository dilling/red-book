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

  extension [A](pa: Par[A]) def map[B](f: A => B): Par[B] =
    pa.map2(unit(()))((a, _) => f(a))
        
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] { def call = a(es).get })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = f andThen lazyUnit

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = 
    ps.foldRight(Par.unit(List.empty[A]))((a1, a2) => a1.map2(a2)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    parMap(as)(a => if (f(a)) then Some(a) else None).map(_.flatten)
