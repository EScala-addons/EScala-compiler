/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime.types;

import scala.Type;
import scala.Array;

public class SingleType extends Type {
    private final Object instance;

    public SingleType(Object instance) {
        this.instance = instance;
    }

    public Array newArray(int size) {
        throw new Error();      // TODO
    }

    public Object defaultValue() {
        return null;
    }

    public boolean isInstance(Object o) {
        return o == instance;
    }

    public boolean isSubType(Type that) {
        return that.isInstance(instance);
    }

    public boolean isSameAs(Type that) {
        return (that instanceof SingleType)
            && (this.instance == ((SingleType)that).instance);
    }

    public int hashCode() {
        return System.identityHashCode(instance);
    }
}
