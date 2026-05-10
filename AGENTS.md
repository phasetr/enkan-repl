# Repository Guidelines

## Project Structure & Modules

- Core: `enkan-repl.el` plus helpers like `enkan-repl-utils.el`, `hmenu.el`.
- Terminal backend abstraction:
  - `enkan-repl-terminal.el` — dispatch + eat backend + tmux backend
  - `enkan-repl-state.el` — disk persistence + tmux session reconcile
- Workspace: `enkan-repl-workspace.el`, `enkan-repl-workspace-list.el`.
- Sessions: `enkan-repl-sessions.el`.
- macOS-only notifications: `enkan-repl-mac-notify.el`.
- Tests: `test/*.el` ERT suites (e.g., `enkan-repl-core-test.el`).
- Docs & tooling: `docs/` (gitignored design notes), `scripts/`
  (doc/constant generators, test loader), `Makefile`.
- Examples: `examples/*.el` for usage patterns.
- Node tooling only for releases (`package.json`, `semantic-release`).

## Build, Test, and Development Commands

- `make help`: List available targets.
- `make install-deps`: Install Emacs dev deps (package-lint from MELPA).
- `make test`: Run ERT tests.
- `make compile`: Byte-compile with warnings-as-errors.
- `make checkdoc`: Check docstrings and headers.
- `make lint`: Run `package-lint` on `enkan-repl.el`.
- `make format`: Indent and untabify key files via Emacs.
- `make check`: Run tests, compile, checkdoc, lint, format.
- `make docs`: Generate constants then public API docs.
Tip: Select a specific Emacs with `EMACS=/path/to/emacs make check`.

## Coding Style & Naming Conventions

- Language: Emacs Lisp; 2-space indentation, spaces only (`.editorconfig`).
- Naming: Public symbols prefixed `enkan-repl-`; tests mirror names with `*-test.el`.
- Docstrings: Required for all public functions/vars; keep checkdoc clean.
- Linting: Fix `package-lint` and byte-compile warnings before PR.
- Formatting: Run `make format` before committing.

## Testing Guidelines

- Framework: ERT; put tests in `test/` and name with `ert-deftest enkan-…`.
- Running: `make test` or `make check`.
- Scope: Prefer pure, deterministic tests; avoid network and external side effects.
- CI: Tests run on Emacs 30.1 (see `.github/workflows/ci.yml`).

## Commit & Pull Request Guidelines

- Commits: Use Conventional Commits (e.g., `feat: add region sender`, `fix: escape handling on macOS`).
- Normal route: update `main`, create a topic branch, make an initial empty
  commit, and open a pull request before continuing with substantive changes.
- PRs: Describe intent, link issues, include test notes and doc updates if applicable.
- Requirements: `make check` passes; no `.elc` files; keep diffs focused.
- Releases: Automated via semantic-release on `main`; do not bump versions manually.

## Security & Configuration

- No secrets required for local development. Node is only for maintainers’ release pipeline.
- Tests must not execute destructive commands; prefer temporary buffers and stubs.
