;;; mod-llm.el --- LLM and agent integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Multi-provider LLM support (Claude, GPT, Gemini, Ollama), agentic workflows,
;; and inline completions via GitHub Copilot.

;;; Code:

;; gptel - multi-provider LLM client
(use-package gptel
  :commands (gptel gptel-send gptel-menu gptel-rewrite)
  :general
  (llmacs/leader-keys
    "l" '(:ignore t :wk "llm")
    "ll" '(gptel :wk "chat")
    "ls" '(gptel-send :wk "send")
    "lm" '(gptel-menu :wk "menu")
    "lr" '(gptel-rewrite :wk "rewrite"))
  :config
  ;; Claude backend
  (gptel-make-anthropic "Claude"
    :stream t
    :key #'gptel-api-key-from-auth-source)

  ;; OpenAI backend
  (gptel-make-openai "GPT"
    :stream t
    :key #'gptel-api-key-from-auth-source)

  ;; Default to Claude
  (setq gptel-model 'claude-sonnet-4-20250514
        gptel-default-mode 'org-mode))

;; agent-shell - agentic workflows
(use-package agent-shell
  :commands agent-shell
  :general
  (llmacs/leader-keys
    "la" '(agent-shell :wk "agent")))

;; GitHub Copilot - inline completions
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :hook (prog-mode . copilot-mode)
  :general
  (:keymaps 'copilot-completion-map
   :states 'insert
   "TAB" #'copilot-accept-completion
   "C-n" #'copilot-next-completion
   "C-p" #'copilot-previous-completion
   "C-g" #'copilot-clear-overlay)
  :config
  (setq copilot-indent-offset-warning-disable t))

(provide 'mod-llm)
;;; mod-llm.el ends here
