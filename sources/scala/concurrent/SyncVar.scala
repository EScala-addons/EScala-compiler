package scala.concurrent;

class SyncVar[a]() extends Monitor() {
  private var isDefined: Boolean = False;
  private var value: a = _;
  def get = synchronized {
    if (!isDefined) wait();
    value
  }
  def set(x: a) = synchronized {
    value = x ; isDefined = True ; notifyAll();
  }
  def isSet: Boolean = 
    isDefined;
  def unset = synchronized { 
    isDefined = False; 
  }
}

