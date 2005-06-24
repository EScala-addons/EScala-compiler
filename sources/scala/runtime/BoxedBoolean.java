/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.runtime;

public final class BoxedBoolean {

    private final static BoxedBoolean TRUE = new BoxedBoolean(true);
    private final static BoxedBoolean FALSE = new BoxedBoolean(false);

    public static BoxedBoolean box(boolean value) {
	return (value ? TRUE : FALSE);
    }

    public final boolean value;

    private BoxedBoolean(boolean value) { this.value = value; }
  
    public final boolean booleanValue() { return value; }

    public boolean equals(java.lang.Object other) {
	return other instanceof BoxedBoolean && value == ((BoxedBoolean) other).value;
    }

    public int hashCode() {
	return value ? 1 : 0;
    }
    
    public String toString() {
	return String.valueOf(value);
    }
}


