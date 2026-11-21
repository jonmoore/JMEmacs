#!/bin/bash
# macOS setup for JMEmacs: installs the tools the config expects (via Homebrew)
# and clones the repo to ~/git/JMEmacs.  Run with: bash etc/macos-setup.sh
# Safe to re-run.
set -e

# 1. Xcode Command Line Tools: git plus the C compiler jinx needs to build its module.
if ! xcode-select -p >/dev/null 2>&1; then
  xcode-select --install
  echo ">> Finish the Command Line Tools install dialog, then run this script again."
  exit 0
fi

# 2. Homebrew
if ! command -v brew >/dev/null 2>&1; then
  if [ -x /opt/homebrew/bin/brew ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
  else
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    eval "$(/opt/homebrew/bin/brew shellenv)"
  fi
fi

# 3. Tools referenced by the config
brew install git ripgrep enchant pkg-config graphviz go
brew install p7zip || brew install sevenzip || echo ">> 7-Zip not installed (only needed for zip extraction)"

# 4. Tools for optional features (each degrades quietly if missing)
# pandoc: ox-pandoc export.  cmake, libtool: compiling vterm's module.
brew install pandoc cmake libtool || echo ">> some optional tools not installed"

# go-grip: Markdown preview (installed to ~/go/bin, which emacs.el adds to exec-path)
go install github.com/chrishrb/go-grip@latest || echo ">> go-grip not installed"

# LaTeX for AUCTeX: BasicTeX (~100 MB) rather than the full MacTeX (~5 GB),
# plus latexmk.  The installer and tlmgr ask for your password.
if [ ! -x /Library/TeX/texbin/tlmgr ]; then
  brew install --cask basictex || echo ">> BasicTeX not installed"
fi
if [ -x /Library/TeX/texbin/tlmgr ] && [ ! -x /Library/TeX/texbin/latexmk ]; then
  sudo /Library/TeX/texbin/tlmgr update --self
  sudo /Library/TeX/texbin/tlmgr install latexmk
fi

# 5. Directories the config uses
mkdir -p ~/git ~/backup ~/info

# 6. Clone (or update) the config
if [ -d ~/git/JMEmacs/.git ]; then
  git -C ~/git/JMEmacs pull --ff-only
else
  git clone https://github.com/jonmoore/JMEmacs.git ~/git/JMEmacs
fi

echo ">> Done. Start Emacs; the first launch downloads packages from ELPA/MELPA and takes a few minutes."
