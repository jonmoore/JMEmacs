# CLAUDE.md

A personal Emacs configuration, used on macOS, Linux and Windows.

## Layout

- `emacs.el`: the main configuration.  Each machine's `~/.emacs.d/init.el`
  is a small bootstrap that loads it.
- `early-init.el`: settings that must run before packages are activated.
  Each machine's `~/.emacs.d/early-init.el` loads it.
- `emacs-custom.el`: the `custom-file`.  `package-selected-packages` is
  not saved here: `emacs.el` builds it from the use-package declarations
  each session, so `package-autoremove` removes undeclared packages.
- `lisp/`: personal and vendored helper libraries.  `personal-autoloads.el`
  is generated from `;;;###autoload` cookies by `update-personal-autoloads.el`
  (using `loaddefs-generate`) and is git-ignored.  It is regenerated when a
  file in `lisp/` changes or is added, removed or renamed.
- `etc/package-manifest.el`: lists the installed version and source commit
  of every package, since package.el can't pin versions.  CI runs it after
  the tests: the list goes in each job's summary and is uploaded unzipped
  as the `package-manifest-<OS>.txt` artifact, which opens in the browser.
  GitHub keeps both for 90 days.
- `etc/macos-setup.sh`: installs the external tools the config uses on a
  Mac.  Run it from the checkout.
- `test/`: the test suite (below).
- `.github/`: CI workflow and Dependabot configuration.

## Tests

Run everything from the repository root, on any platform:

    emacs --batch -l test/run-all.el

Save the output with `> test/last-run.log 2>&1` (`test/*.log` is ignored).
Each suite runs in its own Emacs:

- `test-units.el` (`-Q`): ERT tests for helpers in `lisp/`.  Add tests when
  changing helpers, especially before switching a file to lexical binding.
- `test-environment.el` (`-Q`): required external tools are on `exec-path`;
  optional ones are reported.
- `test-lint.el --strict`: byte-compiles `lisp/*.el` into a temp directory.
  Any compiler warning fails.
- `test-startup.el`: loads `early-init.el` and `emacs.el`, then requires every
  package declared with `use-package` so deferred `:config` blocks run.  Fails
  on any error (including ones that are caught and reported) or
  `display-warning` at level `:error`.  Packages that stop to ask a question
  in batch mode are reported as skipped.  It never starts a server, saves the
  desktop or writes `emacs-custom.el`.

To test a fresh install without touching `~/.emacs.d`:

    emacs --batch --init-directory=SOME-EMPTY-DIR -l test/test-startup.el

## Conventions

- Keep tooling platform-independent: tests and helpers are Elisp run with
  `emacs --batch`, not shell scripts or Makefiles.  OS-specific code goes
  behind `system-osx-p`, `system-linux-p`, `system-win32-p` or `system-wsl-p`.
- No XEmacs-specific code.
- Lint stays at zero warnings.  Fix the cause; use `declare-function` or
  `defvar` for things defined elsewhere; use `with-suppressed-warnings`, with
  a comment, only where the suggested replacement might behave differently.
- Use `cl-lib` (`cl-...`), never `cl`.  Prefer built-ins such as `seq-*`.
- Files in `lisp/` carry `lexical-binding: nil` cookies to keep their existing
  behaviour.  Switch a file to `t` deliberately, with tests.
- Prefer behaviour-preserving changes; call out any behaviour change.

## Workarounds to revisit

- `nael-lsp`'s autoloads call `keymap-set` on `nael-mode-map` before `nael`
  is loaded.  `early-init.el` and the code around `package-initialize` in
  `emacs.el` give them a throwaway keymap; the `nael` use-package binds
  `C-c C-l`.  Remove all three once nael-lsp is fixed upstream.

## CI

- `.github/workflows/test.yml` runs `test/run-all.el` on Ubuntu, macOS and
  Windows.  Linux and macOS get Emacs from `purcell/setup-emacs`; Windows
  downloads GNU's official zip from `ftpmirror.gnu.org`.
- The macOS job installs its tools by running `etc/macos-setup.sh`, so a
  change to that script is tested there.  Linux and Windows install theirs
  in the workflow.
- Third-party actions are pinned to full commit SHAs, and the workflow token
  is read-only (`permissions: contents: read`).  Keep it that way.
- `~/.emacs.d/elpa` is cached per OS, Emacs version and month, keyed on
  `emacs.el`.  The first run each month installs every package afresh, so
  CI picks up new package versions.
- Dependabot opens weekly PRs for action updates.
- Useful commands: `gh run list`, `gh run view <id> --log-failed`,
  `gh run watch`.
