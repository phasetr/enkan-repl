# Contributing to claudemacs-repl

Thank you for your interest in contributing to claudemacs-repl!

## Development Setup

1. Clone the repository
2. Install Node.js dependencies: `npm install`
3. Run tests: `make check`

## Commit Message Format

This project uses [Conventional Commits](https://www.conventionalcommits.org/) for automated versioning and changelog generation.

### Format

```
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

```
feat: change API structure

BREAKING CHANGE: the API structure has changed significantly
```

## Release Process

Releases are automated using semantic-release:

1. **Patch** (0.0.1): `fix:` commits
2. **Minor** (0.1.0): `feat:` commits  
3. **Major** (1.0.0): `BREAKING CHANGE:` commits

When you push to `main` branch with conventional commits:
- Version is automatically bumped
- CHANGELOG.md is updated
- Git tag is created
- GitHub release is published

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