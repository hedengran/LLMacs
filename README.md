# LLMacs

A minimal, fast-loading Emacs configuration with Evil mode at its core.

Built collaboratively with Claude (LLM) - hence the name. The codebase is structured for easy AI-assisted modifications via `CLAUDE.md`/`AGENTS.md`.

## Principles

1. **Fast startup** - GC tuning, lazy loading, deferred packages
2. **Minimal** - Only install what's needed
3. **Built-in first** - Prefer Emacs built-in features when sufficient
4. **Evil-centric** - Vim keybindings everywhere
5. **Spacemacs-style** - SPC leader key for discoverability
6. **Modular** - Split into logical files for easy navigation

## Requirements

- Emacs 29+ (for built-in eglot, modus themes)
- Git (for straight.el package management)
- ripgrep (optional, for fast project search)
- gopls (for Go development): `go install golang.org/x/tools/gopls@latest`

## Installation

```bash
git clone https://github.com/yourusername/llmacs ~/.llmacs.d
emacs --init-directory=~/.llmacs.d
```

First launch will bootstrap straight.el and install packages.

## Structure

```
~/.llmacs.d/
├── init.el                  # Entry point
├── early-init.el            # Pre-GUI optimizations
├── lisp/                    # Core modules (always loaded)
│   ├── core-packages.el     # straight.el + use-package
│   ├── core-ui.el           # Theme, line numbers
│   ├── core-editor.el       # Basic editing, which-key
│   ├── core-evil.el         # Evil + extensions
│   ├── core-keybindings.el  # SPC leader bindings
│   └── core-completion.el   # Vertico, Consult, Corfu
└── modules/                 # Feature modules
    ├── mod-project.el       # Project management
    ├── mod-git.el           # Magit
    └── mod-go.el            # Go + eglot LSP
```

## Key Bindings

Leader key is `SPC`. Local leader (mode-specific) is `,`.

| Key | Action |
|-----|--------|
| `SPC SPC` | M-x |
| `SPC f f` | Find file |
| `SPC f r` | Recent files |
| `SPC b b` | Switch buffer |
| `SPC p p` | Switch project |
| `SPC p f` | Find file in project |
| `SPC /` | Ripgrep search |
| `SPC g s` | Magit status |
| `SPC c m` | Make (with target completion) |
| `SPC c k` | Kill compilation |
| `SPC w v` | Split vertical |
| `SPC w s` | Split horizontal |
| `SPC h f` | Describe function |
| `SPC q q` | Quit |

### Go mode (local leader)

| Key | Action |
|-----|--------|
| `, g d` | Go to definition |
| `, g r` | Find references |
| `, r r` | Rename symbol |
| `, t t` | Test at point |
| `, t p` | Test package |
| `, x r` | Run file |

### Special buffers

| Key | Action |
|-----|--------|
| `q` | Quit window |
| `C-n` | Next item |
| `C-p` | Previous item |

## Adding a Language

1. Create `modules/mod-<lang>.el`
2. Add major mode package with `:mode` for file extensions
3. Hook `eglot-ensure` for LSP
4. Add local leader bindings for common actions
5. Add `(require 'mod-<lang>)` to init.el

See `modules/mod-go.el` as reference.

## License

MIT
