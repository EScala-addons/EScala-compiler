/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id:PostgreSQL.scala 6853 2006-03-20 16:58:47 +0100 (Mon, 20 Mar 2006) dubochet $


package scala.dbc.vendor;


abstract class PostgreSQL extends Vendor {
  
  def uri:java.net.URI;
  def user:String;
  def pass:String;
  
  val retainedConnections = 5;
  
  val nativeDriverClass = Class.forName("org.postgresql.Driver");
  
  val urlProtocolString = "jdbc:postgresql:"
  
}
