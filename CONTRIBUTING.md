# Contributing to enkan-repl

Thank you for your interest in contributing to enkan-repl!

## Development Setup

1. Clone the repository
2. Install Emacs dependencies: `make install-deps`
3. Run tests: `make check`

> **Note**: Node.js is only required for release automation (handled by GitHub Actions). Local development uses Emacs/ELisp tools only.

## Commit Message Format

This project uses [Conventional Commits](https://www.conventionalcommits.org/) for automated versioning and changelog generation.

### Format

```text
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

### Types

- **feat**: A new feature
- **fix**: A bug fix
- **docs**: Documentation only changes
- **style**: Changes that do not affect the meaning of the code (white-space, formatting, missing semi-colons, etc)
- **refactor**: A code change that neither fixes a bug nor adds a feature
- **perf**: A code change that improves performance
- **test**: Adding missing tests or correcting existing tests
- **chore**: Changes to the build process or auxiliary tools and libraries

### Examples

```bash
feat: add interactive template error handling
fix: resolve newline handling issues on macOS
docs: update README with installation instructions
refactor: improve code formatting and structure
test: add comprehensive error handling tests
chore: update dependencies
```

### Breaking Changes

For breaking changes, add `BREAKING CHANGE:` in the footer:

```text
feat: change API structure

BREAKING CHANGE: the API structure has changed significantly
```

## Release Process

### Current Release System

The project uses **semantic-release** with GitHub Actions for fully automated releases.

### Release Steps

#### Automatic (Recommended)
1. Push conventional commits to `main` branch
2. GitHub Actions triggers automatically
3. Version bumped, CHANGELOG updated, Git tag created, GitHub Release published

#### Manual Override (For Maintainers Only)
1. Install Node.js dependencies: `npm install`
2. Run `npm run semantic-release` locally
3. Requires `GITHUB_TOKEN` environment variable

*Note*: This is only needed for repository maintainers. End users installing via MELPA do not need Node.js.

### Version Bumping Rules

| Commit Type | Version Bump | Example |
|-------------|--------------|---------|
| `fix:` | Patch (0.0.1) | Bug fixes |
| `feat:` | Minor (0.1.0) | New features |
| `BREAKING CHANGE:` | Major (1.0.0) | Breaking changes |

### Files Updated During Release

- `enkan-repl.el` - Version field
- `CHANGELOG.md` - Auto-generated from commits
- `default.org` - Regenerated with new version
- Git tag created
- GitHub Release published

### Required Format

Use Conventional Commits:
```
feat: add new feature
fix: resolve bug
docs: update documentation
```

## Testing

Run the full test suite:

```bash
make check
```

This includes:

- Unit tests (ERT)
- Linting (biome)
- Byte compilation
- Documentation checks

## Code Style

- Follow existing Emacs Lisp conventions
- Use meaningful function and variable names
- Add comprehensive docstrings
- Write tests for new functionality

## Pull Requests

1. Fork the repository
2. Create a feature branch
3. Make your changes with conventional commit messages
4. Ensure all tests pass
5. Submit a pull request

Thank you for contributing!
