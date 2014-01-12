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

SBCL_TARBALL_URL="http://downloads.sourceforge.net/project/sbcl/sbcl/1.1.14/sbcl-1.1.14-x86-64-linux-binary.tar.bz2"
SBCL_TARBALL="sbcl.tar.bz2"
SBCL_DIR="$HOME/sbcl"

function install_sbcl {
    echo "Installing SBCL..."
    get "$SBCL_TARBALL_URL" "$SBCL_TARBALL"
    unpack -j "$SBCL_TARBALL" "$SBCL_DIR"
    install_cl_launch "LISP=sbcl" "SBCL=\"$SBCL_DIR/run-sbcl.sh\""
}

QUICKLISP_URL="http://beta.quicklisp.org/quicklisp.lisp"

function install_quicklisp {
    get "$QUICKLISP_URL" quicklisp.lisp
    echo "Installing Quicklisp..."
    cl-launch -f quicklisp.lisp -i "(quicklisp-quickstart:install)"
}

download_cl_launch
install_sbcl

cl-launch -i '(format t "~%~a ~a up and running!~%~%"
                      (lisp-implementation-type)
                      (lisp-implementation-version))'

install_quicklisp

popd
