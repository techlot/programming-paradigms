import java.util.concurrent.Semaphore

class Airport(val name: String, runways: Int) {

  val semaphore = new Semaphore(runways)

  def check_runways(): Unit = this.synchronized {
    println("[" + name + "] available runways [" + semaphore.availablePermits() + "/" + runways + "]")
    if (semaphore.availablePermits() == 0) {
      println("[" + name + "] plane " + Thread.currentThread().getName + " waiting for landing.")
    }
  }

  def handle_aircraft(plane: Plane): Unit = {
    semaphore.acquire()
    println("[" + name + "] plane " + Thread.currentThread().getName + " arrived.")
    println("[" + name + "] Plane " + Thread.currentThread().getName + " refilling.")
    Thread.sleep(1000 * plane.refill_time)
    if (plane.final_destination) {
      println("[" + name + "] plane " + Thread.currentThread().getName + " parked.")
      semaphore.release()
      plane.stop()
    }
    println("[" + name + "] plane " + Thread.currentThread().getName + " departed.")
    semaphore.release()
  }
}

class Plane(val name: String, val refill_time: Int, val route: List[Airport], var final_destination: Boolean) extends Thread(name) {
  override def run(): Unit = {
    println("Plane " + name + " is in the air.")
    for (airport <- 0 until route.length) {
      route(airport).check_runways()
      if (airport == route.length - 1) {
        final_destination = true
      }
      route(airport).handle_aircraft(this)
    }
  }
}

object AirportSimulation {
  def main(args: Array[String]): Unit = {

    val a1 = new Airport("Warsaw", 2)
    val a2 = new Airport("London", 2)
    val a3 = new Airport("Moscow", 1)

    val p1 = new Plane("Z", 2, List(a1, a2), false) // Warsaw -> London
    val p2 = new Plane("X",3, List(a3), false)      // Moscow
    val p3 = new Plane("Y",2, List(a1, a3), false)  // Warsaw -> Moscow
    val p4 = new Plane("W", 3, List(a2, a3), false) // London -> Moscow

    p1.start()
    p2.start()
    p3.start()
    p4.start()

    /*
        val p5 = new Plane("Z", 2, List(a3, a2, a1), false)
        val p6 = new Plane("X",3, List(a3, a1, a2, a3), false)
        val p7 = new Plane("Y",2, List(a1, a2), false)
        val p8 = new Plane("W", 3, List(a2, a3, a2), false)
        p5.start()
        p6.start()
        p7.start()
        p8.start()
     */
  }
}
