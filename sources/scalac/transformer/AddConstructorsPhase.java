/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

//package scala.compiler.backend;
package scalac.transformer;

import java.util.HashMap;

import scalac.Global;
import scalac.Phase;
import scalac.PhaseDescriptor;
import scalac.CompilationUnit;
import scalac.symtab.Symbol;
import scalac.symtab.Type;
import scalac.util.Debug;

public class AddConstructorsPhase extends Phase {

    //########################################################################
    // Private Fields

    /** A map from old constructor symbols to new ones */
    private final HashMap/*<Symbol,Symbol>*/ constructors = new HashMap();

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public AddConstructorsPhase(Global global, PhaseDescriptor descriptor) {
        super(global, descriptor);
    }

    //########################################################################
    // Public Methods

    /** Applies this phase to the given type for the given symbol. */
    public Type transformInfo(Symbol symbol, Type type) {
        if (symbol.isConstructor()) {
            switch (type) {
            case PolyType(Symbol[] tparams, MethodType(_, Type result)):
                result = Type.MethodType(Symbol.EMPTY_ARRAY, result);
                return Type.PolyType(tparams, result);
            case MethodType(_, Type result):
                return Type.MethodType(Symbol.EMPTY_ARRAY, result);
            default:
                throw Debug.abort("illegal case", type);
            }
        }
        return type;
    }

    /** Applies this phase to the given compilation unit. */
    public void apply(CompilationUnit unit) {
        new AddConstructors(global, constructors).apply(unit);
    }

    //########################################################################
}
