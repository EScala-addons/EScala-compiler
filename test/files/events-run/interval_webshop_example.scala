import scala.events._
import scala.collection.mutable._

object SimpleWebShop {
	
	
	
	/***
	 * Constraints:
	 *  1. While paying, the shopping card is read only
	 *  2. Only a user which is logged in can pay
	 *  3. After paying the shopping card is empty
	 *  4. After loggoff the shopping card is empty
	 *  5. Login does not affect the shopping card
	 */
	trait WebShop {
		
		private val shoppingCard = new ListBuffer[String]()
		
		def login(usr : String)
		def logoff
		def addToCard(article : String)
		def payRequest
		def confirm
		
		def getShoppingCard() : List[String] =  {
			shoppingCard.toList
		}
		
		protected def displayLoginPage() {
			
		}
		
		/***
		 * This Method should be called if the Booking was successful. After
		 * that method the shopping card is empty
		 */
		protected def bookingSuccessful {
			shoppingCard .clear()
		}
		
		protected def clearShoppingCard {
			shoppingCard .clear()
		}
		
		/***
		 * Adds a element to the shopping card
		 */
		protected def addToShoppingcard(article : String) {
			shoppingCard += article
		}
	}
	
	class EventDrivenWebShop extends WebShop {
		val loginEvt = new ImperativeEvent[String]
		val logoffEvt = new ImperativeEvent[Unit]
		val addToCardEvt = new ImperativeEvent[String]
		val payRequestEvt = new ImperativeEvent[Unit]
		val confirmEvt = new ImperativeEvent[Unit]
		
		
		val loggedIn = new BetweenEvent(loginEvt, logoffEvt)
		
		val currentlyPaying : IntervalEvent[Unit] = new BetweenEvent(payRequestEvt.within(loggedIn), confirmEvt)
		val authenticationRequested = new BetweenEvent(payRequestEvt.not_within(loggedIn), loginEvt)
		
		val paymentProcessFinishedEvt = confirmEvt.within(currentlyPaying)
		
		val shopping = currentlyPaying .complement 
		val addToCardAllowedEvt = addToCardEvt.within(shopping)
		
		addToCardAllowedEvt += {(article : String) => addToShoppingcard(article)}
		paymentProcessFinishedEvt += {(u : Unit) => bookingSuccessful}
		authenticationRequested .after += {(v:Any) => payRequestEvt()}
		loggedIn.after += {(v:Any) => clearShoppingCard}
		
		
		/**
		 * The following methods consist only to implement the interface required for the 
		 * abstract base class
		 */
		def login(usr:String) {
			loginEvt(usr)
		}
		
		def logoff {
			logoffEvt()
		}
		
		def addToCard(article : String) {
			addToCardEvt(article)
		}
		
		def payRequest {
			payRequestEvt()
		}
		
		def confirm {
			confirmEvt()
		}

		override def toString = "EventDrivenWebShop"
	}
	
	class StateDrivenWebShop extends WebShop {
		
		object LoginState extends Enumeration
		{
			type LoginState = Value
			val NotLoggedIn, LoggedIn = Value
		}
		
		object UserSessionState extends Enumeration {
			type UserSessionState = Value
			val BrowseProducs, DisplayLogin, ConfirmPaymentMethod, ShopppingSuccessful = Value
		}
		
		var userState = LoginState.NotLoggedIn
		var sessionState = UserSessionState.BrowseProducs 
		
		def login(usr: String) {
			if(userState == LoginState.NotLoggedIn )
			{
				userState  =  LoginState.LoggedIn
			}
		}
		
		def logoff {
			if(userState == LoginState.LoggedIn ) {
				userState = LoginState.NotLoggedIn 
				clearShoppingCard
			}
		}
		
		def addToCard(article : String) {
			addToShoppingcard(article)
		}
		
		def payRequest {
			if(userState == LoginState.NotLoggedIn){
				displayLoginPage()				
			}
			sessionState  = UserSessionState.ConfirmPaymentMethod
		}
		
		def confirm {
			if(sessionState == UserSessionState.ConfirmPaymentMethod  && userState == LoginState.LoggedIn ) {
				bookingSuccessful
				sessionState  = UserSessionState.BrowseProducs 
			}
		}
		
		override def toString = "StateDrivenWebShop"
	}
	
	class WebShopTest(shop : WebShop) {
		
		def expect(expected : List[String], actual : List[String])  {
			if(! actual.equals(expected))
			{ 
				throw new RuntimeException("Failed: Expected " + expected + " but was "+ actual)
			}
		}
		
		def login_add_pay {
			shop.login("frank")
			shop.addToCard("melk")
			shop.addToCard("soap")
			shop.addToCard("salt")
			
			expect(shop.getShoppingCard , List("melk", "soap", "salt"))
			
			shop.payRequest
			shop.confirm
			
			expect(shop.getShoppingCard, List())
			
			println(shop + " login_add_pay ok")
		}
		
		def add_pay_login_pay {
			shop.addToCard("melk")
			shop.addToCard("ice")
			
			expect(shop.getShoppingCard, List("melk", "ice"))
			
			shop.payRequest
			shop.confirm
			
			expect(shop.getShoppingCard, List("melk", "ice"))
			
			shop.login("frank")
			shop.confirm
			
			expect(shop.getShoppingCard, List())
			println(shop + " add_pay_login_pay ok")
		}
		
		def login_add_logoff_pay_login {
			shop.login("frank")
			shop.addToCard("melk")
			shop.logoff
			
			expect(shop.getShoppingCard, List())

			shop.addToCard("ink")
			shop.payRequest
			shop.login("frank")
			
			expect(shop.getShoppingCard, List("ink"))
			
			shop.confirm
			expect(shop.getShoppingCard, List())
			println(shop + " login_add_logoff_pay_login ok")
		}
	}
	
  def main(args : Array[String]) : Unit = 
  {
	  new WebShopTest(new EventDrivenWebShop).login_add_pay
	   
	   new WebShopTest(new EventDrivenWebShop).add_pay_login_pay
	   
	   new WebShopTest(new EventDrivenWebShop).login_add_logoff_pay_login
	   
	   new WebShopTest(new StateDrivenWebShop).login_add_pay
	   
	   new WebShopTest(new StateDrivenWebShop).add_pay_login_pay
	   
	   new WebShopTest(new StateDrivenWebShop).login_add_logoff_pay_login
  }
 
}
