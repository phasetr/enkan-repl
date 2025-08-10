# [0.8.0](https://github.com/phasetr/enkan-repl/compare/v0.7.0...v0.8.0) (2025-08-10)


### Features

* Add custom window navigation command ([51517a7](https://github.com/phasetr/enkan-repl/commit/51517a715510d72d86344df11078eccf9c9f0168))

# [0.7.0](https://github.com/phasetr/enkan-repl/compare/v0.6.1...v0.7.0) (2025-08-10)


### Features

* Add keybinding management and window layout examples ([0c6bc44](https://github.com/phasetr/enkan-repl/commit/0c6bc44d7cee79d9243e294fcbba4e5871b22512))

## [0.6.1](https://github.com/phasetr/enkan-repl/compare/v0.6.0...v0.6.1) (2025-08-09)


### Bug Fixes

* preserve internal empty lines and leading whitespace in send functions ([0034406](https://github.com/phasetr/enkan-repl/commit/0034406e0d9eec67246760c7913deb8ddeac1238))

# [0.6.0](https://github.com/phasetr/enkan-repl/compare/v0.5.0...v0.6.0) (2025-08-09)


### Features

* migrate from claudemacs to eat terminal backend ([09d6517](https://github.com/phasetr/enkan-repl/commit/09d651715c6247370065780f9f0a57aad3a09166))


### BREAKING CHANGES

* claudemacs dependency replaced with eat

- Add enkan-repl-start-eat and enkan-repl-finish-eat functions
- Update buffer detection to support *enkan:* naming pattern
- Replace ALL claudemacs references with eat/session throughout codebase
- Add enkan-repl-list-sessions for session management with d/q keys
- Update package dependencies from claudemacs to eat
- Remove all backward compatibility code and claudemacs functions
- Update constants and test files to reference eat instead of claudemacs
- Fix all test file references from claudemacs to eat/enkan
- Update mock functions and test assertions for eat backend

This is a minor version bump as it adds new functionality while
maintaining the same public API surface (renamed functions).

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>

* feat: improve window management for eat buffer display

- Display eat buffer in appropriate window based on current layout
  - Single window: split horizontally, show eat on right
  - Two windows: use right window for eat (like setup-window-layout)
  - Three+ windows: split input file window horizontally
- Keep focus on input file after starting eat session
- Add declare-function for eat to avoid byte-compile warnings

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>

* feat: fix window management for eat buffer display

- Ensure eat buffer is displayed in correct target window
- Fix window selection logic for 1, 2, and 3+ window configurations
- Add comprehensive window management tests
- Fix syntax errors and paren balance

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>

* fix: ensure eat buffer displays in right window and keeps focus

- Fix two-window layout to use right window like setup-window-layout
- Keep focus on eat buffer after startup for immediate interaction
- Update tests to verify correct focus behavior

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>

* fix: ensure eat buffer displays in right window and keeps focus

- Fix eat buffer window management to display in right window
- Maintain focus on org input buffer after eat buffer creation
- Ensure window stays split and focus remains stable

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>

* fix: return focus to org buffer after creating eat session

eatバッファを右ウィンドウに作成後、元のorgバッファにフォーカスを戻すように修正

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>

* feat: add enkan-repl-recenter-bottom function

eatバッファのカーソルを最下段に移動する関数を実装。
ウィンドウが表示されている場合はselect-windowを使用し、
表示されていない場合はwith-current-bufferを使用。

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>

* fix: fix recenter error in enkan-repl-recenter-bottom

select-windowでウィンドウに切り替えた後、with-current-bufferで
バッファコンテキストを確実に設定してからrecenterを実行するように修正。
これにより「'recenter'ing a window that does not display current-buffer」
エラーを回避。

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>

* fix: return focus to original window after recenter-bottom

enkan-repl-recenter-bottom実行後、カーソルを元のウィンドウ（入力ファイル）に戻すように修正

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>

* fix: keep eat buffer cursor at bottom after sending text

send系関数でテキスト送信後、eatバッファのカーソルを最下段（point-max）に
移動するように修正。これによりカーソルが上に飛ぶ問題を解決。

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>

* fix: keep eat buffer cursor at bottom after sending text using async timer

テキスト送信後にeatバッファのカーソルが上に飛ぶ問題を修正。
run-at-timeを使用して非同期でカーソルを最下段に移動するように変更。

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>

* refactor: pure function approach for session list management

セッションリスト機能を純粋関数と副作用のある関数に分離：
- 純粋関数: extract-session-info, collect-sessions, format-sessions, find-deletion-bounds
- 副作用関数: get-buffer-info-list, display-sessions-in-buffer
- 削除後の即座のリスト更新を実装
- 15個の純粋関数テストを追加して堅牢性を向上

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>

* fix: remove interactive from internal function and update constants

- enkan-repl--delete-session-at-pointから(interactive)を削除（内部関数のため）
- constants ファイルを再生成（21個の公開関数を含む）
- enkan-repl-recenter-bottomが正しくcheatsheetに含まれることを確認

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>

* feat: refactor session list to use interactive completing-read UI

Replace buffer-based session list display with interactive completing-read
interface matching the cheatsheet UI pattern. Users can now select sessions
interactively and choose to switch or delete with confirmation.

Key changes:
- Remove buffer-based display functions (enkan-repl--display-sessions-in-buffer)
- Remove internal delete function (enkan-repl--delete-session-at-point)
- Implement completing-read with annotations for session details
- Add action selection (switch/delete/cancel) after session selection
- Update constants file with new function descriptions

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>

* feat: simplify session list with numbered selection

Replace completing-read with simple numbered list selection to avoid
complex minibuffer keymap issues. Users now select sessions by number
and then choose action (switch/delete/quit).

Key changes:
- Remove complex minibuffer keymap configuration
- Implement simple numbered list display (one-line format)
- Add pure function for formatted session list generation
- Remove non-functional eat cursor tests
- Add comprehensive tests for new functionality

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>

* claude destroys codes completely

* fix: return cursor to input file after starting eat session

After starting eat session, cursor now returns to the original input file window
instead of staying in the eat buffer. This provides better workflow continuity.

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>

* refactor: rename cheatsheet to cheat-sheet for consistency

Changed all occurrences of 'cheatsheet' to 'cheat-sheet' across the codebase
for better readability and consistency with compound word conventions.

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>

* refactor: update list-sessions to use minibuffer interface like cheat-sheet

Changed enkan-repl-list-sessions to use completing-read with annotations
instead of numbered selection. Now shows sessions in minibuffer with
directory and status info, then prompts for action after selection.

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>

* docs: update README with eat terminal backend information

Updated README.org to reflect the switch from claudemacs to eat terminal
emulator as the backend, and clarified the package's philosophy of
supporting multiple AI CLIs through terminal emulation.

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>

* fix: correct documentation generation flow order

Changed the build flow to generate constants before docs:
- Makefile: constants generation now runs before docs generation
- .releaserc.js: same order adjustment for release process
- Updated enkan-repl-constants.el with latest function descriptions

This ensures documentation can potentially use the generated constants.

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>

* fix: restore dynamic Core Functions generation and fix compilation warnings

- Fixed generate-docs.el to dynamically generate Core Functions from constants
- Added missing function declarations to avoid byte-compile warnings
- Core Functions section now automatically reflects actual functions in code
- Updated README.org with dynamically generated Core Functions

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>

# [0.5.0](https://github.com/phasetr/claudemacs-repl/compare/v0.4.0...v0.5.0) (2025-08-07)


### Bug Fixes

* correct repository URLs to fix semantic-release ([2cec2da](https://github.com/phasetr/claudemacs-repl/commit/2cec2da55bdeae768fa1de3ae2308d99d1904449))


### Features

* rename package from claudemacs-repl to enkan-repl ([d87e423](https://github.com/phasetr/claudemacs-repl/commit/d87e423dce44451f3d36fcbd6b03b5fdffc3b511))


### BREAKING CHANGES

* Package renamed from claudemacs-repl to enkan-repl

# [0.4.0](https://github.com/phasetr/enkan-repl/compare/v0.3.0...v0.4.0) (2025-08-06)


### Features

* implement enkan-repl-finish-claudemacs command ([cc8c103](https://github.com/phasetr/enkan-repl/commit/cc8c10377f3a7e9f13db3fbdb7029e762984b825))

# [0.3.0](https://github.com/phasetr/enkan-repl/compare/v0.2.3...v0.3.0) (2025-08-06)


### Features

* add interactive cheat-sheet command for function discovery ([#7](https://github.com/phasetr/enkan-repl/issues/7)) ([2c3b140](https://github.com/phasetr/enkan-repl/commit/2c3b1405346eb82450230812da5dbf6b8b68d208))


### BREAKING CHANGES

* None (backward compatible)

## [0.2.3](https://github.com/phasetr/enkan-repl/compare/v0.2.2...v0.2.3) (2025-08-05)


### Bug Fixes

* replace string-empty-p with zerop for Emacs 28.2 compatibility ([18f5d26](https://github.com/phasetr/enkan-repl/commit/18f5d26d3258c897cd7411d63bf93e5062a07a11))
* trigger release for previous documentation restoration work ([38fec41](https://github.com/phasetr/enkan-repl/commit/38fec417f05b831906b9785bbd168488e292e75b))

## [0.2.2](https://github.com/phasetr/enkan-repl/compare/v0.2.1...v0.2.2) (2025-08-04)


### Bug Fixes

* resolve default.org path resolution for straight package installs ([#4](https://github.com/phasetr/enkan-repl/issues/4)) ([19d2fc7](https://github.com/phasetr/enkan-repl/commit/19d2fc76998592e026f409b950b4f5ac5ab1a115))

## [0.2.1](https://github.com/phasetr/enkan-repl/compare/v0.2.0...v0.2.1) (2025-08-04)


### Bug Fixes

* trigger release for cl-return error fix ([571c064](https://github.com/phasetr/enkan-repl/commit/571c06432ce9d418c6b7eef24c7538ad33da3b6b))

# [0.2.0](https://github.com/phasetr/enkan-repl/compare/v0.1.0...v0.2.0) (2025-08-04)


### Bug Fixes

* replace string-join with mapconcat for Emacs 28.2 compatibility ([4fc79e6](https://github.com/phasetr/enkan-repl/commit/4fc79e62102240b72acbf6124282c4cdd970d6b0))


### Features

* add ESC key sending functionality ([#2](https://github.com/phasetr/enkan-repl/issues/2)) ([1929cba](https://github.com/phasetr/enkan-repl/commit/1929cba5f3dc3e361a1bbf302e3d536205c5908b))

# 1.0.0 (2025-08-04)


### Bug Fixes

* add check-ci target for CI environments without package-lint ([2cce0ad](https://github.com/phasetr/enkan-repl/commit/2cce0ad16e46cd19c0174b8f994a7d134f94f6e4))
* add GitHub Actions permissions for semantic-release ([5d944f4](https://github.com/phasetr/enkan-repl/commit/5d944f4628e763c384855928a1943fc058fdcdc7))
* add missing conventional-changelog-conventionalcommits dependency ([4b3f438](https://github.com/phasetr/enkan-repl/commit/4b3f438b5cb11f2558adce7c33766cb6f817197d))
* remove npm cache configuration to resolve CI failure ([8936b6a](https://github.com/phasetr/enkan-repl/commit/8936b6a8e4357ee3922d7772dfdc5b299e0e5d4f))
* remove package-lint from CI check target ([3359626](https://github.com/phasetr/enkan-repl/commit/33596264c8ff0e4861c89a4aaa23faaf9ee181bb))
* replace string-empty-p with length check for Emacs 28.2 compatibility ([ebe3efd](https://github.com/phasetr/enkan-repl/commit/ebe3efdd93a07874bd19460ea89d036b2f855b5e))
* reset package version to 0.0.0 for proper 0.x versioning ([1741dca](https://github.com/phasetr/enkan-repl/commit/1741dca628d4d7dc28d97dae5f30d1f586e1dc7c))
* resolve quality check warnings and add automation ([cafe67d](https://github.com/phasetr/enkan-repl/commit/cafe67d4f9bd96010a6168646350b7c458d06e24))


### Features

* comprehensive release automation and development improvements ([#1](https://github.com/phasetr/enkan-repl/issues/1)) ([fd3c050](https://github.com/phasetr/enkan-repl/commit/fd3c050633b978a07ca239443e31b1decabebd7d))
* configure semantic-release for 0.x versioning ([37d692a](https://github.com/phasetr/enkan-repl/commit/37d692ae5b676f6c35f8aaee69581a52317e61b6))

# 1.0.0 (2025-08-04)


### Bug Fixes

* add check-ci target for CI environments without package-lint ([2cce0ad](https://github.com/phasetr/enkan-repl/commit/2cce0ad16e46cd19c0174b8f994a7d134f94f6e4))
* add GitHub Actions permissions for semantic-release ([5d944f4](https://github.com/phasetr/enkan-repl/commit/5d944f4628e763c384855928a1943fc058fdcdc7))
* remove npm cache configuration to resolve CI failure ([8936b6a](https://github.com/phasetr/enkan-repl/commit/8936b6a8e4357ee3922d7772dfdc5b299e0e5d4f))
* remove package-lint from CI check target ([3359626](https://github.com/phasetr/enkan-repl/commit/33596264c8ff0e4861c89a4aaa23faaf9ee181bb))
* replace string-empty-p with length check for Emacs 28.2 compatibility ([ebe3efd](https://github.com/phasetr/enkan-repl/commit/ebe3efdd93a07874bd19460ea89d036b2f855b5e))
* resolve quality check warnings and add automation ([cafe67d](https://github.com/phasetr/enkan-repl/commit/cafe67d4f9bd96010a6168646350b7c458d06e24))


### Features

* comprehensive release automation and development improvements ([#1](https://github.com/phasetr/enkan-repl/issues/1)) ([fd3c050](https://github.com/phasetr/enkan-repl/commit/fd3c050633b978a07ca239443e31b1decabebd7d))
* configure semantic-release for 0.x versioning ([37d692a](https://github.com/phasetr/enkan-repl/commit/37d692ae5b676f6c35f8aaee69581a52317e61b6))

# 1.0.0 (2025-08-04)


### Bug Fixes

* add check-ci target for CI environments without package-lint ([2cce0ad](https://github.com/phasetr/enkan-repl/commit/2cce0ad16e46cd19c0174b8f994a7d134f94f6e4))
* add GitHub Actions permissions for semantic-release ([5d944f4](https://github.com/phasetr/enkan-repl/commit/5d944f4628e763c384855928a1943fc058fdcdc7))
* remove npm cache configuration to resolve CI failure ([8936b6a](https://github.com/phasetr/enkan-repl/commit/8936b6a8e4357ee3922d7772dfdc5b299e0e5d4f))
* remove package-lint from CI check target ([3359626](https://github.com/phasetr/enkan-repl/commit/33596264c8ff0e4861c89a4aaa23faaf9ee181bb))
* replace string-empty-p with length check for Emacs 28.2 compatibility ([ebe3efd](https://github.com/phasetr/enkan-repl/commit/ebe3efdd93a07874bd19460ea89d036b2f855b5e))
* resolve quality check warnings and add automation ([cafe67d](https://github.com/phasetr/enkan-repl/commit/cafe67d4f9bd96010a6168646350b7c458d06e24))


### Features

* comprehensive release automation and development improvements ([#1](https://github.com/phasetr/enkan-repl/issues/1)) ([fd3c050](https://github.com/phasetr/enkan-repl/commit/fd3c050633b978a07ca239443e31b1decabebd7d))

# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### [Unreleased] Added

- Quality check automation script
- Code quality improvements and warning fixes

### Fixed

- Byte compilation warnings
- Checkdoc documentation style warnings
- Unused variable warnings

## [0.0.0] - 2025-08-02

### [0.0.0] Added

- Initial release of enkan-repl
- Enhanced text sending functionality (region, buffer, cursor-to-end)
- Dedicated input buffer with org-mode support
- History management for sent content
- Multi-session claudemacs buffer detection
- Efficient key bindings for workflow optimization
- Global key binding (C-M-d) for quick access
- MIT License compatibility with upstream claudemacs
- Comprehensive documentation and usage examples
