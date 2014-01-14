#!/bin/bash

set -e

pushd "$HOME"

# get <url> <destination>
function get {
    url=$1
    destination=$2
    echo "Downloading ${url}..."
    curl --no-progress-bar --retry 10  -o "$destination" -L "$url"
}

# unpack <uncompression option> <file> <destination>
function unpack {
    opt=$1
    file=$2;
    destination=$3;

    echo "Unpacking tarball..."
    mkdir "$destination"
    tar -C "$destination" --strip-components=1 "$opt" -xf "$file"
}

function install_i386_arch {
    # Travis-CI's dpkg doesn't seem to know about --add-architecture.
    #sudo dpkg --add-architecture i386
    #sudo apt-get update
    sudo apt-get install libc6:i386
}

CL_LAUNCH_URL="http://common-lisp.net/project/xcvb/cl-launch/cl-launch.tar.gz"
CL_LAUNCH_DIR="$HOME/cl-launch"
CL_LAUNCH_TARBALL="$HOME/cl-launch.tar.gz"
CL_LAUNCH_SCRIPT="/usr/local/bin/cl-launch"
CL_LAUNCH_RC="$HOME/.cl-launchrc"

function download_cl_launch {
    get "$CL_LAUNCH_URL" "$CL_LAUNCH_TARBALL"
    unpack -z "$CL_LAUNCH_TARBALL" "$CL_LAUNCH_DIR"
}

# install_cl_launch <lisp> <option>
function install_cl_launch {
    echo "Installing cl-launch to $CL_LAUNCH_SCRIPT..."

    rm -f "$CL_LAUNCH_RC"
    for arg; do
        echo $arg >> "$CL_LAUNCH_RC"
    done
    
    sudo bash "$CL_LAUNCH_DIR/cl-launch.sh" \
        -I "$CL_LAUNCH_DIR" \
        -o "$CL_LAUNCH_SCRIPT" \
        --rc \
        --init '(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                                       (user-homedir-pathname))))
                  (when (probe-file quicklisp-init)
                    (load quicklisp-init)))' \
        -B install
}

# install_script <path> <lines...>
function install_script {
    path=$1; shift
    tmp=$(mktemp)

    echo "#!/bin/sh" > "$tmp"
    for line; do
        echo "$line" >> "$tmp"
    done
    chmod 755 "$tmp"

    sudo mv "$tmp" "$path"
}

ABCL_TARBALL_URL="http://www.abcl.org/releases/1.2.1/abcl-bin-1.2.1.tar.gz"
ABCL_TARBALL="abcl.tar.gz"
ABCL_DIR="$HOME/abcl"
ABCL_SCRIPT="/usr/local/bin/abcl"

function install_abcl {
    sudo apt-get install default-jre
    get "$ABCL_TARBALL_URL" "$ABCL_TARBALL"
    unpack -z "$ABCL_TARBALL" "$ABCL_DIR"

    install_script "$ABCL_SCRIPT" \
        "java -cp \"$ABCL_DIR/abcl-contrib.jar\" \
              -jar \"$ABCL_DIR/abcl.jar\" \"\$@\""

    install_cl_launch "LISP=abcl"
}

SBCL_TARBALL_URL="http://downloads.sourceforge.net/project/sbcl/sbcl/1.1.14/sbcl-1.1.14-x86-64-linux-binary.tar.bz2"
SBCL_TARBALL="sbcl.tar.bz2"
SBCL_DIR="$HOME/sbcl"

function install_sbcl {
    echo "Installing SBCL..."
    get "$SBCL_TARBALL_URL" "$SBCL_TARBALL"
    unpack -j "$SBCL_TARBALL" "$SBCL_DIR"
    ( cd "$SBCL_DIR" && sudo bash install.sh )
    install_cl_launch "LISP=sbcl"
}

CCL_TARBALL_URL="ftp://ftp.clozure.com/pub/release/1.9/ccl-1.9-linuxx86.tar.gz"
CCL_TARBALL="ccl.tar.gz"
CCL_DIR="$HOME/ccl"
CCL_SCRIPT="/usr/local/bin/ccl"

function install_ccl {
    echo "Installing CCL..."
    get "$CCL_TARBALL_URL" "$CCL_TARBALL"
    unpack -z "$CCL_TARBALL" "$CCL_DIR"
    install_script "$CCL_SCRIPT" "\"$CCL_DIR/lx86cl64\" \"\$@\""
    install_cl_launch "LISP=ccl"
}

CMUCL_TARBALL_URL="http://common-lisp.net/project/cmucl/downloads/snapshots/2014/01/cmucl-2014-01-x86-linux.tar.bz2"
CMUCL_EXTRA_TARBALL_URL="http://common-lisp.net/project/cmucl/downloads/snapshots/2014/01/cmucl-2014-01-x86-linux.extra.tar.bz2"
CMUCL_TARBALL="cmucl.tar.bz2"
CMUCL_EXTRA_TARBALL="cmucl-extra.tar.bz2"
CMUCL_DIR="$HOME/cmucl"
CMUCL_SCRIPT="/usr/local/bin/cmucl"

function install_cmucl {
    echo "Installing CMUCL..."
    install_i386_arch
    get "$CMUCL_TARBALL_URL" "$CMUCL_TARBALL"
    get "$CMUCL_EXTRA_TARBALL_URL" "$CMUCL_EXTRA_TARBALL"
    mkdir -p "$CMUCL_DIR"
    tar -C "$CMUCL_DIR" -xjf "$CMUCL_TARBALL"
    tar -C "$CMUCL_DIR" -xjf "$CMUCL_EXTRA_TARBALL"

    install_script "$CMUCL_SCRIPT" \
        "CMUCLLIB=\"$CMUCL_DIR/lib/cmucl/lib\" \"$CMUCL_DIR/bin/lisp\" \"\$@\""

    install_cl_launch "LISP=cmucl"
}

ECL_TARBALL_URL="http://common-lisp.net/~loliveira/tarballs/ecl-13.5.1-linux-amd64.tar.gz"
ECL_TARBALL="ecl.tar.gz"

function install_ecl {
    echo "Installing ECL..."
    get "$ECL_TARBALL_URL" "$ECL_TARBALL"
    sudo tar -C / -xzf "$ECL_TARBALL"
    install_cl_launch "LISP=ecl"
}

# version of ASDF known to work with cl-launch
ASDF_URL="https://raw.github.com/sbcl/sbcl/sbcl-1.1.14/contrib/asdf/asdf.lisp"

function install_clisp {
    echo "Installing CLISP..."
    sudo apt-get install clisp
    get "$ASDF_URL" asdf.lisp
    echo "(load \"$HOME/asdf.lisp\")" > "$HOME/.clisprc.lisp"
    # tweaking CLISP_OPTIONS so that ASDF gets loaded via the RC file.
    install_cl_launch "LISP=clisp" "CLISP_OPTIONS=\"--quiet --quiet\""
}

QUICKLISP_URL="http://beta.quicklisp.org/quicklisp.lisp"

function install_quicklisp {
    get "$QUICKLISP_URL" quicklisp.lisp
    echo "Installing Quicklisp..."
    cl-launch -f quicklisp.lisp -i "(quicklisp-quickstart:install)"
}

download_cl_launch

case "$LISP" in
    abcl) install_abcl ;;
    sbcl) install_sbcl ;;
    ccl) install_ccl ;;
    cmucl) install_cmucl ;;
    clisp) install_clisp ;;
    ecl) install_ecl ;;
    *)
        echo "Unrecognised lisp: '$LISP'"
        exit 1
        ;;
esac

cl-launch -i '(format t "~%~a ~a up and running!~%~%"
                      (lisp-implementation-type)
                      (lisp-implementation-version))'

install_quicklisp

popd
