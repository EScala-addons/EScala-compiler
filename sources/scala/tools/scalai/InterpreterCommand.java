/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: InterpreterCommand.java,v 1.3 2002/09/02 12:58:22 paltherr Exp $
// $Id$

package scalai;

import scalac.PhaseRepository;
import scalac.CompilerCommand;
import scalac.util.Reporter;
import scalac.util.BooleanOptionParser;
import scalac.util.ScalaProgramArgumentParser;

public class InterpreterCommand extends CompilerCommand {

    //########################################################################
    // Public Fields

    public final BooleanOptionParser interactive;
    public final BooleanOptionParser emacs;
    public final ScalaProgramArgumentParser program;

    //########################################################################
    // Public Constructors

    public InterpreterCommand(String product, String version,
        Reporter reporter, PhaseRepository phases)
    {
        this(product, version, "<source files> [-- <module> <args>]",
            reporter, phases);
    }

    public InterpreterCommand(String product, String version, String syntax,
        Reporter reporter, PhaseRepository phases)
    {
        super(product, version, syntax, reporter, phases);

        this.interactive = new BooleanOptionParser(this,
            "interactive", "Start interpreter in interactive mode",
            false);

        this.emacs = new BooleanOptionParser(this,
            "emacs", "Use Emacs editing mode",
            false);

        this.program = new ScalaProgramArgumentParser(this);

        remove(outpath);
        remove(target);
        remove(jaco);

        add(0, interactive);
        add(1, emacs);
        add(parsers().indexOf(unknown_options), program);
    }

    //########################################################################
}
