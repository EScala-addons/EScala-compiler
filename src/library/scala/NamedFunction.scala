package scala

/**
 * This trait is intended to be mixed in with one of the FunctionN trait. It allows to
 * compare function objects to check function equality.
 * 
 * @author Lucas Satabin
 *
 */
trait NamedFunction {
	
	/**
	 * The name used to compare with another NamedFunction. Basically it
	 * should be something like <code>method(T_1, .., T_2)</code>
	 */
	def name(): String
	
	/**
	 * The owner object for this function
	 */
	def owner(): Any
	
	/**
	 * Two NamedFunction are equal if the have the same owner object instance and
	 * the same name.
	 */
	override def equals(other: Any) = {
		other match {
			case that: NamedFunction =>
				(this.owner, that.owner) match {
					case (owner1: AnyRef, owner2: AnyRef) =>
						// if the owners are references, check, the reference identity
						this.name == that.name && (owner1 eq owner2)
					case _ => this.name == that.name && (this.owner == that.owner) 
				}
			case _ => false
		}
	}
	
	override def hashCode = 41 * ( 41 + name.hashCode) + owner.hashCode
	
}
