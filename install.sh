#!/bin/sh

set -e

# get <url> <destination>
get() {
    url=$1
    destination=$2
    echo "Downloading ${url}..."
    curl --no-progress-bar --retry 10  -o "$destination" -L "$url"
}

# unpack <uncompression option> <file> <destination>
unpack() {
    opt=$1
    file=$2;
    destination=$3;

    echo "Unpacking tarball..."
    mkdir "$destination"
    tar -C "$destination" --strip-components=1 "$opt" -xf "$file"
}

install_i386_arch() {
    # Travis-CI's dpkg doesn't seem to know about --add-architecture.
    #sudo dpkg --add-architecture i386
    #sudo apt-get update
    sudo apt-get install libc6:i386
}

# version of ASDF known to work with cl-launch
ASDF_URL="https://raw.github.com/sbcl/sbcl/sbcl-1.1.14/contrib/asdf/asdf.lisp"

CL_LAUNCH_URL="http://common-lisp.net/project/xcvb/cl-launch/cl-launch.tar.gz"
CL_LAUNCH_DIR="$HOME/cl-launch"
CL_LAUNCH_TARBALL="$HOME/cl-launch.tar.gz"
CL_LAUNCH_SCRIPT="/usr/local/bin/cl-launch"
CL_LAUNCH_RC="$HOME/.cl-launchrc"

download_cl_launch() {
    get "$CL_LAUNCH_URL" "$CL_LAUNCH_TARBALL"
    unpack -z "$CL_LAUNCH_TARBALL" "$CL_LAUNCH_DIR"
}

# install_cl_launch <lisp> <option>
install_cl_launch() {
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

ASDF_SR_CONF_DIR="$HOME/.config/common-lisp/source-registry.conf.d"
ASDF_SR_CONF_FILE="$ASDF_SR_CONF_DIR/cl-travis.conf"
LOCAL_LISP_TREE="$HOME/lisp"

setup_asdf_source_registry() {
    mkdir -p "$LOCAL_LISP_TREE"
    mkdir -p "$ASDF_SR_CONF_DIR"

    echo "(:tree \"$TRAVIS_BUILD_DIR/\")" > "$ASDF_SR_CONF_FILE"
    echo "(:tree \"$LOCAL_LISP_TREE/\")" >> "$ASDF_SR_CONF_FILE"

    echo "Created $ASDF_SR_CONF_FILE"
    cat -n "$ASDF_SR_CONF_FILE"
}

# install_script <path> <lines...>
install_script() {
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

install_abcl() {
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

install_sbcl() {
    echo "Installing SBCL..."
    get "$SBCL_TARBALL_URL" "$SBCL_TARBALL"
    unpack -j "$SBCL_TARBALL" "$SBCL_DIR"
    ( cd "$SBCL_DIR" && sudo bash install.sh )
    install_cl_launch "LISP=sbcl"
}

SBCL32_TARBALL_URL="http://downloads.sourceforge.net/project/sbcl/sbcl/1.0.58/sbcl-1.0.58-x86-linux-binary.tar.bz2"
SBCL32_TARBALL="sbcl32.tar.bz2"
SBCL32_DIR="$HOME/sbcl32"

install_sbcl32() {
    echo "Installing 32-bit SBCL..."
    install_i386_arch

    get "$SBCL32_TARBALL_URL" "$SBCL32_TARBALL"
    unpack -j "$SBCL32_TARBALL" "$SBCL32_DIR"
    ( cd "$SBCL32_DIR" && sudo bash install.sh )
    sudo ln -s /usr/local/bin/sbcl /usr/local/bin/sbcl32

    get "$ASDF_URL" asdf.lisp
    echo "(load \"$HOME/asdf.lisp\")" > "$HOME/.sbclrc"
    # tweaking SBCL_OPTIONS so that ASDF gets loaded via the RC file.
    install_cl_launch "LISP=sbcl" "SBCL_OPTIONS='--noinform --disable-debugger'"
}

CCL_TARBALL_URL="ftp://ftp.clozure.com/pub/release/1.9/ccl-1.9-linuxx86.tar.gz"
CCL_TARBALL="ccl.tar.gz"
CCL_DIR="$HOME/ccl"
CCL_SCRIPT_PREFIX="/usr/local/bin"

install_ccl() {
    if [ "$LISP" = "ccl32" ]; then
        echo "Installing 32-bit CCL..."
        install_i386_arch
        bin="lx86cl"
        script="ccl32"
    else
        echo "Installing CCL..."
        bin="lx86cl64"
        script="ccl"
    fi
    get "$CCL_TARBALL_URL" "$CCL_TARBALL"
    unpack -z "$CCL_TARBALL" "$CCL_DIR"

    install_script "$CCL_SCRIPT_PREFIX/$script" "\"$CCL_DIR/$bin\" \"\$@\""
    install_cl_launch "LISP=ccl" "CCL=\"$script\""
}

CMUCL_TARBALL_URL="http://common-lisp.net/project/cmucl/downloads/snapshots/2014/01/cmucl-2014-01-x86-linux.tar.bz2"
CMUCL_EXTRA_TARBALL_URL="http://common-lisp.net/project/cmucl/downloads/snapshots/2014/01/cmucl-2014-01-x86-linux.extra.tar.bz2"
CMUCL_TARBALL="cmucl.tar.bz2"
CMUCL_EXTRA_TARBALL="cmucl-extra.tar.bz2"
CMUCL_DIR="$HOME/cmucl"
CMUCL_SCRIPT="/usr/local/bin/cmucl"

install_cmucl() {
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

install_ecl() {
    echo "Installing ECL..."
    get "$ECL_TARBALL_URL" "$ECL_TARBALL"
    sudo tar -C / -xzf "$ECL_TARBALL"
    install_cl_launch "LISP=ecl"
}

install_clisp() {
    if [ "$LISP" = "clisp32" ]; then
        echo "Installing 32-bit CLISP..."
        sudo apt-get remove libsigsegv2
        sudo apt-get install libsigsegv2:i386
        sudo apt-get install clisp:i386
        sudo ln -s /usr/bin/clisp /usr/local/bin/clisp32
    else
        echo "Installing CLISP..."
        sudo apt-get install clisp
    fi
    get "$ASDF_URL" asdf.lisp
    echo "(load \"$HOME/asdf.lisp\")" > "$HOME/.clisprc.lisp"
    # tweaking CLISP_OPTIONS so that ASDF gets loaded via the RC file.
    install_cl_launch "LISP=clisp" "CLISP_OPTIONS=\"--quiet --quiet\""
}

QUICKLISP_URL="http://beta.quicklisp.org/quicklisp.lisp"

install_quicklisp() {
    get "$QUICKLISP_URL" quicklisp.lisp
    echo "Installing Quicklisp..."
    cl-launch -f quicklisp.lisp -i "(quicklisp-quickstart:install)"
}

(
    cd "$HOME"

    download_cl_launch

    case "$LISP" in
        abcl) install_abcl ;;
        sbcl) install_sbcl ;;
        sbcl32) install_sbcl32 ;;
        ccl|ccl32) install_ccl ;;
        cmucl) install_cmucl ;;
        clisp|clisp32) install_clisp ;;
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
    setup_asdf_source_registry
)
