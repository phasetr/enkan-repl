# claudemacs-client

Enhanced client utilities for claudemacs - extending AI pair programming capabilities in Emacs.

[![CI](https://github.com/phasetr/claudemacs-client/workflows/CI/badge.svg)](https://github.com/phasetr/claudemacs-client/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Design Philosophy

This package provides minimal documentation and maximum customization freedom for advanced Emacs users who:
- Have a strong will to use Claude Code outside of officially supported VSCode
- Prefer to configure their own efficient workflows
- Define their own transient menus and keybinding preferences
- Or are familiar with `send-region` patterns (like F# REPL `C-c C-r`)

Welcome to the freedom of Emacs world - we provide the tools, you create the experience.

### Author's Personal Approach

"I cannot remember keybindings unless I use them daily, and transient menus are even more hopeless. From the early prototype stage of this package, I have not set any keybindings and only remember `send-region` and `send-from-cursor`, executing them via `M-x` with fuzzy search. When using Claude Code, I'm focused and only use these functions, so they always appear at the top of the completion list and can be executed immediately."

This personal workflow is the foundation for our function-name-first design philosophy. Common keybindings like `C-c C-r` for send-region are widely used, but we leave all efficiency optimizations to your expertise.

### Core Design Principles

**File-based vs Buffer-based**
- Claude Code's limitation: difficult to preserve personal comments and context
- Avoid building complex history features - use files for persistence instead
- Files allow complete preservation of past comments and thought processes
- Add content chronologically at the bottom, use `send-region` or `send-from-cursor` to send specific parts
- When inspiration strikes after Claude Code execution, write it down in the file - no more forgotten ideas

**Workflow Flexibility**
- Always use `send-region`? Write new comments below the current content
- Always use `send-from-cursor`? Write notes slightly above the cursor position
- File persistence means your workflow adapts to your thinking patterns

**Filename-Buffer Mapping**
- File-local variables and parsing file contents are tedious
- Use filename encoding for simple, unambiguous claudemacs buffer correspondence
- Ugly but functional: filename directly encodes the target directory

**Org-mode Choice**
- This is Emacs - use org-mode, not markdown
- Leverage Emacs's native structured document capabilities

## Features

- 🚀 **Enhanced Text Sending**: Send regions, buffers, or cursor-to-end text to claudemacs
- 📝 **Persistent Input Files**: Org-mode enabled files with multi-language template support
- 🔍 **Directory-Based Targeting**: Automatically match files to appropriate claudemacs buffers (depends on claudemacs buffer naming convention)
- 📁 **Template Customization**: External template files for maximum flexibility

## Installation

### Using straight.el (Recommended)

```elisp
(straight-use-package
  '(claudemacs-client :host github :repo "phasetr/claudemacs-client"))
```

### Manual Installation

1. Download `claudemacs-client.el`
2. Add to your Emacs configuration:

```elisp
;; Add to load path
(add-to-list 'load-path "/path/to/claudemacs-client")

;; Load the package
(require 'claudemacs-client)
```

### Prerequisites

- Emacs 28.1 or later
- [claudemacs](https://github.com/cpoile/claudemacs) installed and configured
- [Claude Code CLI](https://github.com/anthropics/claude-code) setup

## Usage

### Quick Start

1. Start claudemacs in your project directory
2. Run `M-x claudemacs-client-open-project-input`
3. Use the template functions shown in the generated file

### Essential Functions

- `claudemacs-client-open-project-input` - Open or create project input file
- `claudemacs-client-send-region` - Send selected text to claudemacs
- `claudemacs-client-send-from-cursor` - Send text from cursor to end
- `claudemacs-client-send-buffer` - Send entire buffer
- `claudemacs-client-status` - Show connection status and diagnostics

## Configuration

### Template Language

```elisp
;; Use Japanese templates
(setq claudemacs-client-template-language "ja")

;; Use custom template file
(setq claudemacs-client-custom-template-path "~/my-claude-template.org")
```

### Template Customization

Use `M-x claudemacs-client-output-template` to output a standard template to a new buffer for customization.

## Troubleshooting

If you see "❌ Cannot send - no matching claudemacs buffer found for this directory":

1. Ensure claudemacs is running: `M-x claudemacs-transient-menu`
2. Check if Claude Code CLI is properly configured
3. Use `M-x claudemacs-client-status` for diagnostic information

## Development

### Running Tests

```bash
make check
```

## Contributing

This package prioritizes maintainability over feature requests. For template translations or custom workflows, prefer using the `custom` template mechanism rather than contributing to the core package.

Note: Due to leukemia recurrence in June 2024, the maintainer is concerned about potential health deterioration and may have limited capacity for reviewing contributions.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Support

- 🐛 [Issue Tracker](https://github.com/phasetr/claudemacs-client/issues)
- 💬 [Discussions](https://github.com/phasetr/claudemacs-client/discussions)
