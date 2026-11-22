# CLAUDE.md

A personal Emacs configuration for macOS, Linux and Windows. `emacs.el` is
the main file, and `lisp/` holds helper libraries.

## Tests

Run from the repository root:

    emacs --batch -l test/run-all.el

The suite runs unit tests, checks for external tools, loads the full
configuration, runs integration tests against it (key bindings, prompts,
desktop restore, the Python language servers), and lints `lisp/`. Any
error or compiler warning fails it. CI runs the same command on all
three platforms.

## Conventions

- Write tooling in Elisp run with `emacs --batch`, not shell scripts. Put
  OS-specific code behind `system-osx-p`, `system-linux-p`,
  `system-win32-p` or `system-wsl-p`.
- Keep lint at zero warnings by fixing the cause.
- Use `cl-lib`, never `cl`.
- Prefer changes that preserve behaviour, and point out any that don't.
- Don't add `package-selected-packages` to `emacs-custom.el`. `emacs.el`
  builds it from the use-package declarations.
- In the CI workflow, reference GitHub's own actions by version tag and pin
  other third-party actions to commit SHAs.
