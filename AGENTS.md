# AGENTS.md - LLMacs Configuration Guide

This document helps AI agents understand and modify this Emacs configuration.

## Project Overview

LLMacs is a minimal, fast-loading Emacs configuration built on these principles:

1. **Lazy loading** - Packages load only when needed via `use-package :defer`
2. **Fast startup** - Target <1 second load time
3. **Minimal** - Only install what's necessary
4. **Built-in first** - Prefer Emacs built-in features when sufficient
5. **Evil-centric** - Vim keybindings are fundamental
6. **Spacemacs-style** - SPC leader key for discoverability
7. **Modular** - Split into logical files in `lisp/`

## Directory Structure

```
~/.llmacs.d/
├── init.el              # Entry point - loads modules in order
├── early-init.el        # Pre-GUI optimizations (Emacs 27+)
├── AGENTS.md            # This file
├── lisp/                # Core modules (always loaded)
│   ├── core-packages.el     # straight.el + use-package setup
│   ├── core-ui.el           # Visual settings, theme
│   ├── core-editor.el       # Basic editing, which-key
│   ├── core-evil.el         # Evil mode + extensions
│   ├── core-keybindings.el  # SPC leader via general.el
│   └── core-completion.el   # Vertico, Consult, Corfu
└── modules/             # Feature modules
    ├── mod-project.el       # Project management (built-in project.el)
    ├── mod-git.el           # Git via magit + diff-hl
    └── mod-go.el            # Go with eglot LSP
```

## Key Technologies

| Package | Purpose | Why chosen |
|---------|---------|------------|
| straight.el | Package manager | Reproducible, lockfiles, lazy |
| use-package | Config DSL | Declarative, lazy loading |
| evil | Vim emulation | Core requirement |
| evil-collection | Evil in more modes | Consistency |
| general.el | Keybindings | Clean SPC leader support |
| which-key | Key hints | Discoverability |
| vertico | Completion UI | Minimal, fast |
| consult | Search/commands | Integrates with vertico |
| corfu | In-buffer completion | Lighter than company |
| project.el | Project management | Built-in, sufficient |
| magit | Git interface | Best-in-class |
| diff-hl | Git gutter | Fringe indicators |
| go-mode | Go syntax | Standard Go mode |
| eglot | LSP client | Built-in (Emacs 29+) |

## Code Conventions

### Adding a new package

```elisp
(use-package package-name
  ;; :defer t is default - loads lazily
  :hook (some-mode . package-mode)  ; Auto-load trigger
  :general
  (llmacs/leader-keys
    "xx" '(package-command :wk "description"))
  :config
  (setq package-option t))
```

### Adding keybindings

Use `llmacs/leader-keys` for SPC-prefixed bindings:

```elisp
(llmacs/leader-keys
  "p" '(:ignore t :wk "project")  ; Create prefix
  "pf" '(project-find-file :wk "find file"))
```

Use `llmacs/local-leader-keys` for mode-specific (comma-prefixed):

```elisp
(llmacs/local-leader-keys
  :keymaps 'python-mode-map
  "r" '(python-shell-send-region :wk "send region"))
```

### Creating a new module

1. Create `lisp/core-<name>.el` or `modules/<name>.el`
2. Start with the header template:
```elisp
;;; module-name.el --- Description -*- lexical-binding: t; -*-

;;; Commentary:
;; Explain purpose

;;; Code:

;; Configuration here

(provide 'module-name)
;;; module-name.el ends here
```
3. Add `(require 'module-name)` to init.el

## Performance Guidelines

- Always use `:defer t` (default) unless package must load immediately
- Use `:hook` instead of `:init` when possible
- Use `:commands` to specify autoload triggers
- Only use `:demand t` for truly essential packages (evil, general)
- Check startup time: `emacs --debug-init` and startup message

## Current Keybinding Map

### Global (SPC leader)

| Key | Command | Category |
|-----|---------|----------|
| SPC SPC | M-x | - |
| SPC b b | switch buffer | buffer |
| SPC b d | kill buffer | buffer |
| SPC f f | find file | file |
| SPC f r | recent files | file |
| SPC f s | save | file |
| SPC w h/j/k/l | window nav | window |
| SPC w s/v | split h/v | window |
| SPC w d | delete window | window |
| SPC / | ripgrep search | search |
| SPC s s | search buffer | search |
| SPC h f/v/k | describe func/var/key | help |
| SPC q q | quit | quit |
| SPC p p | switch project | project |
| SPC p f | find file in project | project |
| SPC p / | search in project | project |
| SPC p b | project buffer | project |
| SPC p k | kill project buffers | project |
| SPC g s | magit status | git |
| SPC g b | magit blame | git |
| SPC g l | magit log | git |
| SPC g d | magit diff | git |
| SPC g c | magit commit | git |
| SPC g p | magit push | git |
| SPC g F | magit pull | git |
| SPC c m | make (with target completion) | compile |
| SPC c c | compile | compile |
| SPC c r | recompile | compile |
| SPC c k | kill compilation | compile |

### Go mode (comma local leader)

| Key | Command |
|-----|---------|
| , = = | format buffer |
| , g d | go to definition |
| , g r | find references |
| , g i | find implementation |
| , r r | rename symbol |
| , r a | code actions |
| , t t | test at point |
| , t f | test file |
| , t p | test package |
| , x r | go run |
| , x b | go build |

### Special/temporary buffers

All buffers derived from `special-mode` (help, xref, compilation, etc.):

| Key | Command |
|-----|---------|
| q | quit window |
| C-n | next line/error |
| C-p | previous line/error |
| RET | go to item |

## Common Tasks for Agents

### Add a new programming language

1. Create `modules/mod-<lang>.el`
2. Use built-in mode if available, else add package
3. Hook `eglot-ensure` for LSP support
4. Add local-leader bindings (comma prefix) for:
   - `=` format
   - `g` goto (definition, references, implementation)
   - `r` refactor (rename, code actions)
   - `t` test commands
   - `x` execute/run commands
5. Add to evil-collection mode list if needed

Example structure (see `mod-go.el` as reference):
```elisp
(use-package <lang>-mode
  :mode "\\.<ext>\\'"
  :hook ((<lang>-mode . eglot-ensure))
  :general
  (llmacs/local-leader-keys
    :keymaps '<lang>-mode-map
    "==" '(eglot-format-buffer :wk "format")))
```

### Add a new feature module

1. Create `modules/mod-<feature>.el`
2. Add `(require 'mod-<feature>)` to init.el
3. Update AGENTS.md directory structure and keybindings

## Testing Changes

```bash
# Test startup time
emacs --eval='(kill-emacs)'

# Test with debug
emacs --debug-init

# Test in clean env
emacs -Q -l init.el
```
