package a

/**
 * Created by flire on 20.12.15.
 */
object Main_03 {
  import scala.concurrent.ExecutionContext.Implicits.global
  import akka.actor.ActorSystem
  import akka.actor.Props
  import scala.concurrent.duration.DurationInt
  import scala.concurrent.duration.FiniteDuration
  import akka.actor.Actor
  import akka.actor.ActorRef


  object Waiter {
    case class GetLeftFork(philosopher: Int)
    case class GetRightFork(philosopher: Int)
    case class GiveBackBothForks(philosopher: Int)

    case object GiveLeftFork
    case object GiveRightFork
    case object Wait

    private val FORK_UNOCCUPIED = -1
  }

  class Waiter(private val philosophersNumber:Int) extends Actor {
    import Waiter._

    private val forks = Array.fill(philosophersNumber)(FORK_UNOCCUPIED)

    override def receive: Receive = {
      case GetLeftFork(philosopher) =>
        if (isBothForksAvailableFor(philosopher)) {
          forks(philosopher) = philosopher
          sender ! GiveLeftFork
        } else {
          sender ! Wait
        }
      case GetRightFork(philosopher) =>
        if (isBothForksAvailableFor(philosopher)) {
          forks((philosopher + 1) % philosophersNumber ) = philosopher
          sender ! GiveRightFork
        } else {
          sender ! Wait
        }
      case GiveBackBothForks(philosopher) =>
        if (forks(philosopher) == philosopher)
          forks(philosopher) = FORK_UNOCCUPIED
        if (forks((philosopher + 1) % philosophersNumber) == philosopher)
          forks((philosopher + 1) % philosophersNumber) = FORK_UNOCCUPIED
    }

    private def isBothForksAvailableFor(philosopher: Int) =
      isForkAvailable(philosopher, philosopher) && isForkAvailable((philosopher + 1) % philosophersNumber, philosopher)

    private def isForkAvailable(fork: Int, philosopher: Int) =
      forks(fork) == FORK_UNOCCUPIED || forks(fork) == philosopher
  }


  object Philosopher {
    case object Eat
    case object Think
  }

  class Philosopher(private val waiter: ActorRef, private val me: Int) extends Actor{
    import Philosopher._
    import Waiter._

    //implicit private val executionContext = context.dispatcher

    private val eatingTime = 1.seconds
    private val thinkingTime = 3.seconds
    private val retryTime = 50.millis

    private def think() = {
      context.system.scheduler.scheduleOnce(thinkingTime, self, Eat)
      context.become(thinking)
    }

    def hungry: Receive = {
      case Wait =>
        println(s"Philosopher $me is waiting without any forks")
        context.system.scheduler.scheduleOnce(retryTime, waiter, GetLeftFork(me))
      case GiveLeftFork =>
        println(s"Philosopher $me got left fork")
        waiter ! GetRightFork(me)
        context.become(waitingForRightFork)
    }

    def waitingForRightFork: Receive = {
      case Wait =>
        println(s"Philosopher $me is waiting for right fork")
        context.system.scheduler.scheduleOnce(retryTime, waiter, GetRightFork(me))
      case GiveRightFork =>
        println(s"Philosopher $me got right fork")
        println(s"Philosopher $me is eating")
        context.system.scheduler.scheduleOnce(eatingTime, self, Think)
        context.become(eating)
    }

    def thinking: Receive = {
      case Eat =>
        println(s"Philosopher $me is hungry")
        waiter ! GetLeftFork(me)
        context.become(hungry)
    }

    def eating: Receive = {
      case Think =>
        println(s"Philosopher $me finished dining")
        waiter ! GiveBackBothForks(me)
        think()
    }

    def receive: Receive = {
      case Think =>
        println(s"Philosopher $me starts")
        think()
    }

  }

  def main(args: Array[String]): Unit = {
    val philosophersNumber = 3
    val system = ActorSystem()
    val waiter = system.actorOf(Props(classOf[Waiter], philosophersNumber))
    val philosophers = for ( i <- 0 to philosophersNumber - 1 ) yield system.actorOf(Props(classOf[Philosopher], waiter, i))
    philosophers foreach { _ ! Philosopher.Think }
    system.scheduler.scheduleOnce(15.seconds)(system.shutdown())
  }
}
