package miniboxing.plugin

trait MiniboxLogging {
  self: MiniboxLogic =>

  def log(str: => String) = println(str)   
  def debug(str: => String) = () 
}