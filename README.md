# claudemacs-client

Enhanced client utilities for claudemacs - extending AI pair programming capabilities in Emacs.

[![CI](https://github.com/phasetr/claudemacs-client/workflows/CI/badge.svg)](https://github.com/phasetr/claudemacs-client/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Features

- 🚀 **Enhanced Text Sending**: Send regions, buffers, or cursor-to-end text to claudemacs
- 📝 **Persistent Input Files**: Org-mode enabled files for project-specific instructions
- 📚 **History Management**: Track and reuse previously sent content
- 🔍 **Directory-Based Targeting**: Automatically match files to appropriate claudemacs buffers by directory
- ⌨️ **Efficient Keybindings**: Streamlined workflow with intuitive key combinations
- 📁 **Flexible File Management**: Use proper filenames to organize input files anywhere

## Installation

### Manual Installation

1. Download `claudemacs-client.el`
2. Add to your Emacs configuration:

```elisp
;; Add to load path
(add-to-list 'load-path "/path/to/claudemacs-client")

;; Load the package
(require 'claudemacs-client)
```

### Using straight.el (Recommended)

```elisp
(straight-use-package
  '(claudemacs-client :host github :repo "phasetr/claudemacs-client"))
```

### Prerequisites

- Emacs 28.1 or later
- [claudemacs](https://github.com/cpoile/claudemacs) installed and configured
- [Claude Code CLI](https://github.com/anthropics/claude-code) setup

## Usage

### Quick Start

1. Start claudemacs in your project directory
2. Use `C-M-d` to create/open the Claude input file
3. Write your instructions or questions
4. Use `C-c C-c` to send from cursor position

### File Organization Tips

The persistent files use directory-encoded names (e.g., `cec--Users--phasetr--project1.org`). You can:

- **Keep files in project roots** (default) - automatically created per project
- **Centralize in one directory** - set `claudemacs-client-project-file-directory` to organize all files together
- **Create custom files manually** - use the encoding format to match any directory:
  ```
  /Users/myname/work/project/ → cec--Users--myname--work--project.org
  ```
  
This allows flexible management: keep files with projects or organize them centrally while maintaining the directory-to-claudemacs-buffer matching.

### Key Bindings

#### Global
- `C-M-d` - Create/switch to Claude input buffer

#### In any buffer
- `claudemacs-client-send-region` - Send selected region
- `claudemacs-client-send-buffer` - Send entire buffer
- `claudemacs-client-send-from-cursor` - Send from cursor to end

#### In Claude input file
- `C-c C-c` - Send from cursor to end
- `C-c C-r` - Send selected region
- `C-c C-b` - Send entire file
- `C-c C-h` - Insert from history

### Commands

| Command | Description |
|---------|-------------|
| `claudemacs-client-create-input-file` | Create or switch to dedicated input file |
| `claudemacs-client-send-region` | Send selected text to claudemacs |
| `claudemacs-client-send-buffer` | Send entire file content |
| `claudemacs-client-send-from-cursor` | Send text from cursor to end |
| `claudemacs-client-insert-from-history` | Insert previously sent text |
| `claudemacs-client-show-status` | Display connection and history status |

## Configuration

### Customization

```elisp
;; Customize input buffer name
(setq claudemacs-client-input-buffer-name "*My Claude Input*")

;; Adjust history size (default: 50)
(setq claudemacs-client-max-history-size 100)
```

### Custom Key Bindings

```elisp
;; Alternative global binding
(global-set-key (kbd "C-c C-a") #'claudemacs-client-create-input-file)

;; Bind to specific modes
(define-key org-mode-map (kbd "C-c C-s") #'claudemacs-client-send-region)
```

## Examples

### Code Review Request
```org-mode
* Code Review Request

Please review this TypeScript function for:
- Performance issues
- Type safety
- Best practices

** Function to review:
[Select the function and use C-c C-r]
```

### Debugging Session
```org-mode
* Debug Session

I'm getting an error in this Python script:

** Error message:
TypeError: 'NoneType' object is not subscriptable

** Code context:
[Use C-c C-c to send from here]
```

## Troubleshooting

### Connection Issues

If you see "❌ Cannot send - no matching claudemacs buffer found for this directory":

1. Ensure claudemacs is running: `M-x claudemacs-transient-menu`
2. Check if Claude Code CLI is properly configured  
3. Verify the input file's directory encoding matches your claudemacs buffer's directory
4. Use `claudemacs-client-debug-connection` to check directory matching

### Common Issues

**Problem**: Command not recognized
**Solution**: Ensure `claudemacs-client.el` is in your load path and properly required

**Problem**: No text being sent
**Solution**: Check if the claudemacs buffer is active and eat-mode is running

**Problem**: History not working
**Solution**: History is session-based; restart Emacs to clear

## Development

### Running Tests

```bash
# Install dependencies
emacs --batch --eval "(package-install 'buttercup)"

# Run tests
emacs --batch -l claudemacs-client.el -l test/claudemacs-client-test.el -f buttercup-run-discover
```

### Quality Checks

```bash
# Run quality check script
./scripts/quality-check.sh
```

## Roadmap

- [ ] **v0.1.0**: Basic functionality and MELPA submission
- [ ] **v0.2.0**: Multi-project session management
- [ ] **v1.0.0**: Persistent file-based input management
- [ ] **Future**: Integration with projectile and project.el

## Contributing

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/amazing-feature`
3. Make your changes and add tests
4. Run quality checks: `./scripts/quality-check.sh`
5. Commit your changes: `git commit -m 'Add amazing feature'`
6. Push to the branch: `git push origin feature/amazing-feature`
7. Open a Pull Request

## Acknowledgments

This project is built on top of [claudemacs](https://github.com/cpoile/claudemacs) by [cpoile](https://github.com/cpoile). We are grateful for their excellent work in creating the foundation for AI pair programming in Emacs.

Special thanks to:
- The claudemacs community for inspiration and feedback
- The Emacs community for the robust ecosystem
- Anthropic for Claude Code and the AI capabilities

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Support

- 📖 [Documentation](https://github.com/phasetr/claudemacs-client/docs/)
- 🐛 [Issue Tracker](https://github.com/phasetr/claudemacs-client/issues)
- 💬 [Discussions](https://github.com/phasetr/claudemacs-client/discussions)

---

**Made with ❤️ for the Emacs and AI community**
