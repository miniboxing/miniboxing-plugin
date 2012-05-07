package plugin

trait MiniboxLogging {
  self: MiniboxLogic =>

  def log(str: => String) = println(str)   // maybe global.log
  def debug(str: => String) = () // println(str) // maybe global.debuglog
}