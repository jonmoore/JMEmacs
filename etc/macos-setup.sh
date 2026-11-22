#!/bin/bash
# macOS setup for JMEmacs: installs the tools the config expects (via Homebrew).
# Run from the checkout with: bash etc/macos-setup.sh
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

# 3. An `emacs` command for /Applications/Emacs.app: a wrapper script in
#    Homebrew's bin directory, which is on PATH.  (A symlink to the app's
#    binary can stop it finding its own files.)  Only created when there is
#    no `emacs` on PATH yet, so an existing one (or CI's) is left alone.
emacs_app="${JM_EMACS_APP:-/Applications/Emacs.app}"
if ! command -v emacs >/dev/null 2>&1; then
  if [ -x "$emacs_app/Contents/MacOS/Emacs" ]; then
    wrapper="$(brew --prefix)/bin/emacs"
    printf '#!/bin/sh\nexec "%s/Contents/MacOS/Emacs" "$@"\n' "$emacs_app" > "$wrapper"
    chmod +x "$wrapper"
    echo ">> Created $wrapper to run $emacs_app"
  else
    echo ">> No emacs command and no $emacs_app: install Emacs.app, then run this again."
  fi
fi

# 4. Tools referenced by the config
brew install git ripgrep enchant pkg-config graphviz go
brew install p7zip || brew install sevenzip || echo ">> 7-Zip not installed (only needed for zip extraction)"

# 5. Tools for optional features (each degrades quietly if missing)
# pandoc: ox-pandoc export.  cmake, libtool, libvterm: compiling vterm's module.
brew install pandoc cmake libtool libvterm || echo ">> some optional tools not installed"

# agent-shell with Claude: Node.js, the ACP adapter agent-shell starts
# (claude-agent-acp), and the Claude Code CLI, used once to sign in.  Both
# npm packages land in /opt/homebrew/bin, which init.el puts on exec-path.
# Claude Code finishes installing in its postinstall script, which npm only
# runs when allowed; --allow-scripts allows it for that package alone.
brew install node || echo ">> Node.js not installed (needed for agent-shell)"
npm install -g --allow-scripts=@anthropic-ai/claude-code \
    @agentclientprotocol/claude-agent-acp @anthropic-ai/claude-code \
  || echo ">> claude-agent-acp / Claude Code not installed (needed for agent-shell)"

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

# 6. Directories the config uses
mkdir -p ~/git ~/backup ~/info

echo ">> Done.  Next, create ~/.emacs.d/init.el and ~/.emacs.d/early-init.el as"
echo ">> described in README.md (this script doesn't), then start Emacs."
