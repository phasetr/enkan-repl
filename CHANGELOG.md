# [0.4.0](https://github.com/phasetr/claudemacs-repl/compare/v0.3.0...v0.4.0) (2025-08-06)


### Features

* implement claudemacs-repl-finish-claudemacs command ([cc8c103](https://github.com/phasetr/claudemacs-repl/commit/cc8c10377f3a7e9f13db3fbdb7029e762984b825))

# [0.3.0](https://github.com/phasetr/claudemacs-repl/compare/v0.2.3...v0.3.0) (2025-08-06)


### Features

* add interactive cheatsheet command for function discovery ([#7](https://github.com/phasetr/claudemacs-repl/issues/7)) ([2c3b140](https://github.com/phasetr/claudemacs-repl/commit/2c3b1405346eb82450230812da5dbf6b8b68d208))


### BREAKING CHANGES

* None (backward compatible)

## [0.2.3](https://github.com/phasetr/claudemacs-repl/compare/v0.2.2...v0.2.3) (2025-08-05)


### Bug Fixes

* replace string-empty-p with zerop for Emacs 28.2 compatibility ([18f5d26](https://github.com/phasetr/claudemacs-repl/commit/18f5d26d3258c897cd7411d63bf93e5062a07a11))
* trigger release for previous documentation restoration work ([38fec41](https://github.com/phasetr/claudemacs-repl/commit/38fec417f05b831906b9785bbd168488e292e75b))

## [0.2.2](https://github.com/phasetr/claudemacs-repl/compare/v0.2.1...v0.2.2) (2025-08-04)


### Bug Fixes

* resolve default.org path resolution for straight package installs ([#4](https://github.com/phasetr/claudemacs-repl/issues/4)) ([19d2fc7](https://github.com/phasetr/claudemacs-repl/commit/19d2fc76998592e026f409b950b4f5ac5ab1a115))

## [0.2.1](https://github.com/phasetr/claudemacs-repl/compare/v0.2.0...v0.2.1) (2025-08-04)


### Bug Fixes

* trigger release for cl-return error fix ([571c064](https://github.com/phasetr/claudemacs-repl/commit/571c06432ce9d418c6b7eef24c7538ad33da3b6b))

# [0.2.0](https://github.com/phasetr/claudemacs-repl/compare/v0.1.0...v0.2.0) (2025-08-04)


### Bug Fixes

* replace string-join with mapconcat for Emacs 28.2 compatibility ([4fc79e6](https://github.com/phasetr/claudemacs-repl/commit/4fc79e62102240b72acbf6124282c4cdd970d6b0))


### Features

* add ESC key sending functionality ([#2](https://github.com/phasetr/claudemacs-repl/issues/2)) ([1929cba](https://github.com/phasetr/claudemacs-repl/commit/1929cba5f3dc3e361a1bbf302e3d536205c5908b))

# 1.0.0 (2025-08-04)


### Bug Fixes

* add check-ci target for CI environments without package-lint ([2cce0ad](https://github.com/phasetr/claudemacs-repl/commit/2cce0ad16e46cd19c0174b8f994a7d134f94f6e4))
* add GitHub Actions permissions for semantic-release ([5d944f4](https://github.com/phasetr/claudemacs-repl/commit/5d944f4628e763c384855928a1943fc058fdcdc7))
* add missing conventional-changelog-conventionalcommits dependency ([4b3f438](https://github.com/phasetr/claudemacs-repl/commit/4b3f438b5cb11f2558adce7c33766cb6f817197d))
* remove npm cache configuration to resolve CI failure ([8936b6a](https://github.com/phasetr/claudemacs-repl/commit/8936b6a8e4357ee3922d7772dfdc5b299e0e5d4f))
* remove package-lint from CI check target ([3359626](https://github.com/phasetr/claudemacs-repl/commit/33596264c8ff0e4861c89a4aaa23faaf9ee181bb))
* replace string-empty-p with length check for Emacs 28.2 compatibility ([ebe3efd](https://github.com/phasetr/claudemacs-repl/commit/ebe3efdd93a07874bd19460ea89d036b2f855b5e))
* reset package version to 0.0.0 for proper 0.x versioning ([1741dca](https://github.com/phasetr/claudemacs-repl/commit/1741dca628d4d7dc28d97dae5f30d1f586e1dc7c))
* resolve quality check warnings and add automation ([cafe67d](https://github.com/phasetr/claudemacs-repl/commit/cafe67d4f9bd96010a6168646350b7c458d06e24))


### Features

* comprehensive release automation and development improvements ([#1](https://github.com/phasetr/claudemacs-repl/issues/1)) ([fd3c050](https://github.com/phasetr/claudemacs-repl/commit/fd3c050633b978a07ca239443e31b1decabebd7d))
* configure semantic-release for 0.x versioning ([37d692a](https://github.com/phasetr/claudemacs-repl/commit/37d692ae5b676f6c35f8aaee69581a52317e61b6))

# 1.0.0 (2025-08-04)


### Bug Fixes

* add check-ci target for CI environments without package-lint ([2cce0ad](https://github.com/phasetr/claudemacs-repl/commit/2cce0ad16e46cd19c0174b8f994a7d134f94f6e4))
* add GitHub Actions permissions for semantic-release ([5d944f4](https://github.com/phasetr/claudemacs-repl/commit/5d944f4628e763c384855928a1943fc058fdcdc7))
* remove npm cache configuration to resolve CI failure ([8936b6a](https://github.com/phasetr/claudemacs-repl/commit/8936b6a8e4357ee3922d7772dfdc5b299e0e5d4f))
* remove package-lint from CI check target ([3359626](https://github.com/phasetr/claudemacs-repl/commit/33596264c8ff0e4861c89a4aaa23faaf9ee181bb))
* replace string-empty-p with length check for Emacs 28.2 compatibility ([ebe3efd](https://github.com/phasetr/claudemacs-repl/commit/ebe3efdd93a07874bd19460ea89d036b2f855b5e))
* resolve quality check warnings and add automation ([cafe67d](https://github.com/phasetr/claudemacs-repl/commit/cafe67d4f9bd96010a6168646350b7c458d06e24))


### Features

* comprehensive release automation and development improvements ([#1](https://github.com/phasetr/claudemacs-repl/issues/1)) ([fd3c050](https://github.com/phasetr/claudemacs-repl/commit/fd3c050633b978a07ca239443e31b1decabebd7d))
* configure semantic-release for 0.x versioning ([37d692a](https://github.com/phasetr/claudemacs-repl/commit/37d692ae5b676f6c35f8aaee69581a52317e61b6))

# 1.0.0 (2025-08-04)


### Bug Fixes

* add check-ci target for CI environments without package-lint ([2cce0ad](https://github.com/phasetr/claudemacs-repl/commit/2cce0ad16e46cd19c0174b8f994a7d134f94f6e4))
* add GitHub Actions permissions for semantic-release ([5d944f4](https://github.com/phasetr/claudemacs-repl/commit/5d944f4628e763c384855928a1943fc058fdcdc7))
* remove npm cache configuration to resolve CI failure ([8936b6a](https://github.com/phasetr/claudemacs-repl/commit/8936b6a8e4357ee3922d7772dfdc5b299e0e5d4f))
* remove package-lint from CI check target ([3359626](https://github.com/phasetr/claudemacs-repl/commit/33596264c8ff0e4861c89a4aaa23faaf9ee181bb))
* replace string-empty-p with length check for Emacs 28.2 compatibility ([ebe3efd](https://github.com/phasetr/claudemacs-repl/commit/ebe3efdd93a07874bd19460ea89d036b2f855b5e))
* resolve quality check warnings and add automation ([cafe67d](https://github.com/phasetr/claudemacs-repl/commit/cafe67d4f9bd96010a6168646350b7c458d06e24))


### Features

* comprehensive release automation and development improvements ([#1](https://github.com/phasetr/claudemacs-repl/issues/1)) ([fd3c050](https://github.com/phasetr/claudemacs-repl/commit/fd3c050633b978a07ca239443e31b1decabebd7d))

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

- Initial release of claudemacs-repl
- Enhanced text sending functionality (region, buffer, cursor-to-end)
- Dedicated input buffer with org-mode support
- History management for sent content
- Multi-session claudemacs buffer detection
- Efficient key bindings for workflow optimization
- Global key binding (C-M-d) for quick access
- MIT License compatibility with upstream claudemacs
- Comprehensive documentation and usage examples
