########################################################-*-Shell-script-*-####
# Website-Build Function
##############################################################################
# $Id$

source ${0%/*}/stdlib.sh;

##############################################################################
# website-build-distributions

function website-build-distributions-usage() {
    echo "Usage: $program <archive-directory>";
}

function website-build-distributions-args() {
    case "$1" in
        -? | -h | --help      ) $program-usage; exit 0;;
        --version             ) echo "$program (bash script) $version";exit 0;;
        --verbose             ) verbose="true"; return 1;;
        -*                    ) args-option-unknown "$@";;
        *                     ) args-append-array args "$@";;
    esac;
}

function website-build-distributions-get-archive() {
    local archive="$1"; shift 1;

    if [ -f "$archive" ]; then
        local name=`basename $archive`;
        local size=`stat -l -c%s $archive`;
        echo "<archive name=\"$name\" size=\"$size\"/>";
    fi;
}

function website-build-distributions() {
    local program="$FUNCNAME";
    local version='$Revision$';
    local verbose="false";
    local -a args;
    args-loop "$@";

    [ ${#args[@]} == 1 ] || { $program-usage 1>&2; exit 1; };
    local archivedir="${args[0]}";

    if [ ! -d "$archivedir" ]; then
        abort "could not find directory '$archivedir'";
    fi;

    echo "<distributions>";
    echo "";

    local file;
    local archives=`ls -1t $archivedir/*.tar.gz`;
    for file in $archives; do
        local basename=`basename $file .tar.gz`;
        local tgz_file="$archivedir/$basename.tar.gz";
        local bz2_file="$archivedir/$basename.tar.bz2";
        local zip_file="$archivedir/$basename.zip";

        local version_regex="s/[^-]*-\([0-9]*\(\.[0-9]*\.\|-\)[0-9]*\)/\1/";
        local version=`echo $basename | sed "$version_regex"`;
        local date=`date -r "$file" +%d-%b-%Y`;
        local tgz_line=`$program-get-archive $tgz_file`;
        local bz2_line=`$program-get-archive $bz2_file`;
        local zip_line=`$program-get-archive $zip_file`

        echo "  <distribution>";
        echo "    <version>$version</version>";
        echo "    <date>$date</date>";
        echo "    $tgz_line";
        echo "    $bz2_line";
        echo "    $zip_line";
        echo "  </distribution>";
        echo "";
    done;

    echo "</distributions>";
}

##############################################################################
# website-build-installers

function website-build-installers-usage() {
    echo "Usage: $program <archive-basepath>";
}

function website-build-installers-args() {
    case "$1" in
        -? | -h | --help      ) $program-usage; exit 0;;
        --version             ) echo "$program (bash script) $version";exit 0;;
        --verbose             ) verbose="true"; return 1;;
        -*                    ) args-option-unknown "$@";;
        *                     ) args-append-array args "$@";;
    esac;
}

function website-build-installers-add-entry() {
    [ $# = 5 ] || abort "internal error";
    local archive="$1"; shift 1;
    local platform="$1"; shift 1;
    local description="$1"; shift 1;
    local path="$1"; shift 1;
    local anchor="$1"; shift 1;

    if [ ! -f "$archive" ]; then
        warning "could not find file '$archive'";
        return 1;
    fi;

    local size=`stat -l -c%s "$archive"`;
    local size=`echo "scale=1; $size/1024/1024" | bc`;
    case "$size" in .* ) size="0$size";; esac;

    echo "  <installer>";
    [ -n $anchor ] &&
    echo "    <platform>$platform</platform>";
    echo "    <description>$description</description>";
    echo "    <file path=\"$path\" size=\"$size\"/>";
    echo "    <anchor>$anchor</anchor>";
    echo "  </installer>";
    echo "";
}

function website-build-installers-add-installer() {
    [ $# = 4 ] || abort "internal error";
    local installerdir="$1"; shift 1;
    local path="$1"; shift 1;
    local platform="$1"; shift 1;
    local description="$1"; shift 1;

    local path="Web_Installers/InstData/$path"
    local archive="$installerdir/$path"; shift 1;
    $program-add-entry \
        "$archive" "$platform" "$description" "$path" "$platform";
}

function website-build-installers-add-installers() {
    [ $# = 1 ] || abort "internal error";
    local installerdir="$1"; shift 1;

    if [ ! -d "$installerdir" ]; then
        warning "could not find directory '$installerdir'";
        return 1;
    fi;

    local -a add=($program-add-installer "$installerdir");
    "${add[@]}" "Java/install.jar"         "other"   "Java Installer";
    "${add[@]}" "GenericUnix/install.bin"  "unix"    "GenericUnix Installer";
    "${add[@]}" "AIX/NoVM/install.bin"     "aix"     "AIX Installer";
    "${add[@]}" "Linux/NoVM/install.bin"   "linux"   "Linux Installer";
    "${add[@]}" "Solaris/NoVM/install.bin" "solaris" "Solaris Installer";
    "${add[@]}" "HPUX/NoVM/install.bin"    "hp"      "HPUX Installer";
    "${add[@]}" "Windows/NoVM/install.exe" "win"     "Windows Installer";
    "${add[@]}" "MacOSX/install.zip"       "macosx"  "MacOSX Installer";
}

function website-build-installers-add-archive() {
    [ $# = 3 ] || abort "internal error";
    local archive="$1"."$2"; shift 1;
    local suffix="$1"; shift 1;
    local description="$1"; shift 1;

    local path="./distrib/"`basename $archive`;
    $program-add-entry "$archive" "" "$description" "$path" "$suffix";
}

function website-build-installers-add-archives() {
    [ $# = 1 ] || abort "internal error";
    local basepath="$1"; shift 1;

    local -a add=($program-add-archive "$basepath");
    "${add[@]}" "tar.gz"  "Gzip Unix tarball (Unix/Cygwin)";
    "${add[@]}" "tar.bz2" "Bz2 Unix tarball (Unix/Cygwin)";
    "${add[@]}" "zip"     "Zip Archive (Windows)";
}

function website-build-installers() {
    local program="$FUNCNAME";
    local version='$Revision$';
    local verbose="false";
    local -a args;
    args-loop "$@";

    [ ${#args[@]} == 1 ] || { $program-usage 1>&2; exit 1; };
    local basepath="${args[0]}";

    echo "<installers>";
    echo "";
    $program-add-installers "$basepath.ia";
    $program-add-archives   "$basepath";
    echo "</installers>";

    local installhtm="$basepath.ia/Web_Installers/install.htm";
    if [ ! -f "$installhtm" ]; then
        warning "could not find file '$installhtm'";
        return 1;
    fi;

    local start=`grep -n "^setArchiveFile()\$" "$installhtm"`;
    local end=`grep -n "^platformButtons()\$" "$installhtm"`;
    start=$[${start%%:*} + 3];
    end=$[${end%%:*} - 2];

    echo ""
    echo "<params>"
    head -$end "$installhtm" | tail -$[$end - $start] \
        | sed '-es/[	 ]*//' '-es!>$!/>!' '-e/^$/d';
    echo "</params>"
}

##############################################################################
# website-build

function website-build-usage() {
    echo "Usage: $program <archive-directory> <current-version>";
}

function website-build-args() {
    case "$1" in
        -? | -h | --help      ) $program-usage; exit 0;;
        --version             ) echo "$program (bash script) $version";exit 0;;
        --verbose             ) verbose="true"; return 1;;
        -*                    ) args-option-unknown "$@";;
        *                     ) args-append-array args "$@";;
    esac;
}

function website-build() {
    local program="$FUNCNAME";
    local version='$Revision$';
    local verbose="false";
    local -a args;
    args-loop "$@";

    [ ${#args[@]} == 2 ] || { $program-usage 1>&2; exit 1; };
    local archivedir="${args[0]}";
    local current="${args[1]}";

    if [ ! -d "$archivedir" ]; then
        abort "could not find directory '$archivedir'";
    fi;

    $program-distributions "$archivedir";
    echo "";
    $program-installers "$archivedir/scala-$current";
}

##############################################################################
