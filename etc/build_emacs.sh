#!/bin/bash

# Build Emacs from source in WSL with GUI and native compilation support.

# --- Configuration Variables (can be overridden by arguments) ---
EMACS_VERSION_TAG="emacs-30.1" # Or a branch e.g. "master"
BUILD_DIR="$HOME/emacs_build_source"
INSTALL_PREFIX="$HOME/bin/emacs-new" # Default installation directory
NUM_CORES=$(nproc) # Number of CPU cores for compilation

# --- Features ---
ENABLE_GUI=true
ENABLE_NATIVE_COMPILATION=true
ENABLE_TREE_SITTER=true
ENABLE_JSON=true
ENABLE_MODULES=true
ENABLE_GNUTLS=true
ENABLE_IMAGEMAGICK=true
ENABLE_CAIRO=true
ENABLE_MAILUTILS=true
ENABLE_WIDE_INT=true
ENABLE_SOUND=true # libasound2-dev
ENABLE_DBUS=true  # libdbus-1-dev
ENABLE_RSVG=true  # librsvg2-dev
ENABLE_LCMS2=true # liblcms2-dev
ENABLE_LOCKFILE=true # liblockfile-dev

ENABLE_GPM=false   # libgpm-dev (console mouse support)

print_help() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Builds Emacs from source in WSL with GUI and native compilation."
    echo ""
    echo "Options:"
    echo "  -h, --help                        Show this help message and exit."
    echo "  --version <tag>                   Specify Emacs Git tag or branch (default: $EMACS_VERSION_TAG)."
    echo "  --build-dir <path>                Set the build directory (default: $BUILD_DIR)."
    echo "  --prefix <path>                   Set the installation prefix (default: $INSTALL_PREFIX)."
    echo "  --jobs <num_cores>                Number of parallel compilation jobs (default: $NUM_CORES)."
    echo "  --disable-gui                     Disable GUI support (build terminal-only)."
    echo "  --disable-native-compilation      Disable native compilation."
    echo "  --disable-tree-sitter             Disable Tree-sitter support."
    echo "  --disable-json                    Disable JSON support."
    echo "  --disable-modules                 Disable dynamic modules support."
    echo "  --disable-gnutls                  Disable GnuTLS support."
    echo "  --disable-imagemagick             Disable ImageMagick support."
    echo "  --disable-cairo                   Disable Cairo graphics support."
    echo "  --disable-mailutils               Disable Mailutils integration."
    echo "  --disable-wide-int                Disable wide-int support."
    echo "  --disable-sound                   Disable sound support (libasound2-dev)."
    echo "  --disable-dbus                    Disable D-Bus support (libdbus-1-dev)."
    echo "  --disable-gpm                     Disable GPM support (libgpm-dev)."
    echo "  --disable-rsvg                    Disable librsvg2 support (librsvg2-dev)."
    echo "  --disable-lcms2                   Disable lcms2 support (liblcms2-dev)."
    echo "  --disable-lockfile                Disable lockfile support (liblockfile-dev)."
    echo "  --skip-deps                       Skip dependency installation (useful if already installed)."
    echo "  --clean                           Clean the build directory before starting."
    echo "  --no-install                      Perform build but skip 'make install'."
    echo ""
    echo "Example: $0 --version emacs-29.3 --prefix /opt/emacs --jobs 8"
    echo "Example: $0 --disable-gui --disable-native-compilation # Build terminal-only Emacs"
    exit 0
}

# --- Argument Parsing ---
ARGS=$(getopt -o h --long version:,build-dir:,prefix:,jobs:,disable-gui,disable-native-compilation,disable-tree-sitter,disable-json,disable-modules,disable-gnutls,disable-imagemagick,disable-cairo,disable-mailutils,disable-wide-int,disable-sound,disable-dbus,disable-gpm,disable-rsvg,disable-lcms2,disable-lockfile,skip-deps,clean,no-install,help -- "$@")
if [ $? -ne 0 ]; then
    print_help
fi
eval set -- "$ARGS"

SKIP_DEPS=false
CLEAN_BUILD=false
NO_INSTALL=false

while true; do
    case "$1" in
        -h|--help)
            print_help
            ;;
        --version)
            EMACS_VERSION_TAG="$2"
            shift 2
            ;;
        --build-dir)
            BUILD_DIR="$2"
            shift 2
            ;;
        --prefix)
            INSTALL_PREFIX="$2"
            shift 2
            ;;
        --jobs)
            NUM_CORES="$2"
            shift 2
            ;;
        --disable-gui)
            ENABLE_GUI=false
            shift
            ;;
        --disable-native-compilation)
            ENABLE_NATIVE_COMPILATION=false
            shift
            ;;
        --disable-tree-sitter)
            ENABLE_TREE_SITTER=false
            shift
            ;;
        --disable-json)
            ENABLE_JSON=false
            shift
            ;;
        --disable-modules)
            ENABLE_MODULES=false
            shift
            ;;
        --disable-gnutls)
            ENABLE_GNUTLS=false
            shift
            ;;
        --disable-imagemagick)
            ENABLE_IMAGEMAGICK=false
            shift
            ;;
        --disable-cairo)
            ENABLE_CAIRO=false
            shift
            ;;
        --disable-mailutils)
            ENABLE_MAILUTILS=false
            shift
            ;;
        --disable-wide-int)
            ENABLE_WIDE_INT=false
            shift
            ;;
        --disable-sound)
            ENABLE_SOUND=false
            shift
            ;;
        --disable-dbus)
            ENABLE_DBUS=false
            shift
            ;;
        --disable-gpm)
            ENABLE_GPM=false
            shift
            ;;
        --disable-rsvg)
            ENABLE_RSVG=false
            shift
            ;;
        --disable-lcms2)
            ENABLE_LCMS2=false
            shift
            ;;
        --disable-lockfile)
            ENABLE_LOCKFILE=false
            shift
            ;;
        --skip-deps)
            SKIP_DEPS=true
            shift
            ;;
        --clean)
            CLEAN_BUILD=true
            shift
            ;;
        --no-install)
            NO_INSTALL=true
            shift
            ;;
        --)
            shift
            break
            ;;
        *)
            echo "Internal error!"
            exit 1
            ;;
    esac
done

# --- Functions ---

log() {
    echo ">>> $(date +'%Y-%m-%d %H:%M:%S') $1"
}

check_command() {
    if ! command -v "$1" &> /dev/null; then
        log "Error: '$1' command not found. Please ensure essential build tools are installed."
        exit 1
    fi
}

install_dependencies() {
    log "Updating package lists and upgrading installed packages..."
    sudo apt update || { log "Error: apt update failed."; exit 1; }
    sudo apt upgrade -y || { log "Error: apt upgrade failed."; exit 1; }

    log "Installing essential build dependencies..."
    sudo apt install -y build-essential git autoconf texinfo || { log "Error: Failed to install essential dependencies."; exit 1; }

    DEPENDENCIES=""
    $ENABLE_GUI && DEPENDENCIES+=" libgtk-3-dev"
    $ENABLE_NATIVE_COMPILATION && {
        GCC_VERSION=$(gcc -dumpversion | cut -d'.' -f1)
        if [[ -z "$GCC_VERSION" ]]; then
            log "Warning: Could not detect GCC version. Native compilation might fail without 'libgccjit-X-dev'."
            log "Attempting to install common libgccjit and gcc/g++ versions."
            DEPENDENCIES+=" libgccjit0 libgccjit-11-dev libgccjit-12-dev libgccjit-13-dev gcc g++"
        else
            log "Detected GCC version: $GCC_VERSION. Installing libgccjit for this version."
            DEPENDENCIES+=" libgccjit0 libgccjit-${GCC_VERSION}-dev gcc-${GCC_VERSION} g++-${GCC_VERSION}"
        fi
    }
    $ENABLE_TREE_SITTER && DEPENDENCIES+=" libtree-sitter-dev"
    $ENABLE_JSON && DEPENDENCIES+=" libjansson-dev"
    $ENABLE_GNUTLS && DEPENDENCIES+=" libgnutls28-dev"
    $ENABLE_IMAGEMAGICK && DEPENDENCIES+=" libmagick++-dev"
    $ENABLE_CAIRO && DEPENDENCIES+=" libcairo2-dev"
    $ENABLE_MAILUTILS && DEPENDENCIES+=" mailutils"
    $ENABLE_SOUND && DEPENDENCIES+=" libasound2-dev"
    $ENABLE_DBUS && DEPENDENCIES+=" libdbus-1-dev"
    $ENABLE_GPM && DEPENDENCIES+=" libgpm-dev"
    $ENABLE_RSVG && DEPENDENCIES+=" librsvg2-dev"
    $ENABLE_LCMS2 && DEPENDENCIES+=" liblcms2-dev"
    $ENABLE_LOCKFILE && DEPENDENCIES+=" liblockfile-dev"

    DEPENDENCIES+=" libtiff5-dev libgif-dev libjpeg-dev libpng-dev libxpm-dev"
    DEPENDENCIES+=" libncurses-dev"

    if [ -n "$DEPENDENCIES" ]; then
        log "Installing feature-specific dependencies: $DEPENDENCIES"
        for dep in $DEPENDENCIES; do
            sudo apt install -y "$dep" || log "Warning: Failed to install dependency: $dep. Continuing, but build might fail."
        done
    else
        log "No additional feature dependencies selected."
    fi
}

# --- Main Script Logic ---

log "Starting Emacs build script in WSL..."

check_command "git"
check_command "make"
check_command "gcc"
check_command "apt"

if ! $SKIP_DEPS; then
    install_dependencies
else
    log "Skipping dependency installation as requested."
fi

if $CLEAN_BUILD && [ -d "$BUILD_DIR" ]; then
    log "Cleaning existing build directory: $BUILD_DIR"
    rm -rf "$BUILD_DIR" || { log "Error: Failed to clean build directory."; exit 1; }
fi

if [ ! -d "$BUILD_DIR" ]; then
    log "Creating build directory: $BUILD_DIR"
    mkdir -p "$BUILD_DIR" || { log "Error: Failed to create build directory."; exit 1; }
fi

cd "$BUILD_DIR" || { log "Error: Could not change to build directory."; exit 1; }

if [ ! -d "emacs" ]; then
    log "Cloning Emacs source code from Savannah Git repository..."
    git clone https://git.savannah.gnu.org/git/emacs.git || { log "Error: Failed to clone Emacs repository."; exit 1; }
else
    log "Emacs source directory already exists. Pulling latest changes..."
    cd emacs                        || { log "Error: Could not change to emacs source directory."  ; exit 1; }
    git fetch --all                 || { log "Error: Could not run git fetch."                     ; exit 1; }
    cd ..
fi

cd emacs || { log "Error: Could not change to emacs source directory after clone/pull."; exit 1; }

if [ "$EMACS_VERSION_TAG" != "master" ]; then
    log "Checking out Emacs version: $EMACS_VERSION_TAG"
    git checkout "$EMACS_VERSION_TAG" || { log "Error: Failed to checkout specified Emacs version/tag."; exit 1; }
else
    log "Building from latest 'master' branch."
fi

log "Running autogen.sh to generate configure script..."
./autogen.sh || { log "Error: autogen.sh failed."; exit 1; }

log "Configuring Emacs with selected features..."
CONFIGURE_FLAGS=""
if $ENABLE_GUI; then
    CONFIGURE_FLAGS+=" --with-x --with-gtk3 --without-pgtk"
fi
$ENABLE_NATIVE_COMPILATION && CONFIGURE_FLAGS+=" --with-native-compilation"
$ENABLE_TREE_SITTER        && CONFIGURE_FLAGS+=" --with-tree-sitter"
$ENABLE_JSON               && CONFIGURE_FLAGS+=" --with-json"
$ENABLE_MODULES            && CONFIGURE_FLAGS+=" --with-modules"
$ENABLE_GNUTLS             && CONFIGURE_FLAGS+=" --with-gnutls"
$ENABLE_IMAGEMAGICK        && CONFIGURE_FLAGS+=" --with-imagemagick"
$ENABLE_CAIRO              && CONFIGURE_FLAGS+=" --with-cairo"
$ENABLE_MAILUTILS          && CONFIGURE_FLAGS+=" --with-mailutils"
$ENABLE_WIDE_INT           && CONFIGURE_FLAGS+=" --with-wide-int"
$ENABLE_SOUND              && CONFIGURE_FLAGS+=" --with-sound=alsa"
$ENABLE_DBUS               && CONFIGURE_FLAGS+=" --with-dbus"
$ENABLE_GPM                && CONFIGURE_FLAGS+=" --with-gpm"
$ENABLE_RSVG               && CONFIGURE_FLAGS+=" --with-rsvg"
$ENABLE_LCMS2              && CONFIGURE_FLAGS+=" --with-lcms2"
$ENABLE_LOCKFILE           && CONFIGURE_FLAGS+=" --with-lockfile"

CONFIGURE_FLAGS+=" --with-tiff --with-gif --with-jpeg --with-png --with-xpm --with-ncurses"

if $ENABLE_NATIVE_COMPILATION; then
    GCC_BIN=$(which gcc-${GCC_VERSION} || which gcc)
    GXX_BIN=$(which g++-${GCC_VERSION} || which g++)
    if [ -n "$GCC_BIN" ] && [ -n "$GXX_BIN" ]; then
        log "Setting CC=$GCC_BIN CXX=$GXX_BIN for native compilation."
        export CC="$GCC_BIN"
        export CXX="$GXX_BIN"
    else
        log "Warning: Could not find specific GCC/G++ for native compilation. Using default."
    fi
fi

log "Running ./configure $CONFIGURE_FLAGS --prefix=$INSTALL_PREFIX"
./configure $CONFIGURE_FLAGS --prefix="$INSTALL_PREFIX" || { log "Error: configure failed."; exit 1; }

unset CC CXX

log "Compiling Emacs with $NUM_CORES parallel jobs..."
make -j"$NUM_CORES" || { log "Error: make failed."; exit 1; }

if ! $NO_INSTALL; then
    log "Installing Emacs to $INSTALL_PREFIX/bin..."
    sudo make install || { log "Error: make install failed."; exit 1; }
    log "Emacs installed successfully! You can run it with: $INSTALL_PREFIX/bin/emacs"
    log "Consider adding $INSTALL_PREFIX/bin to your PATH."
else
    log "Skipping 'make install' as requested."
    log "Emacs binaries are located in $BUILD_DIR/emacs/src"
fi

log "Emacs build process completed."
log "To run: $INSTALL_PREFIX/bin/emacs"
log "To run in terminal: $INSTALL_PREFIX/bin/emacs -nw"

