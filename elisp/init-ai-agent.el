;;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package aidermacs
  :ensure (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :custom
  (aidermacs-backend 'comint)
  (aidermacs-auto-commits nil)
  (aidermacs-default-chat-mode 'architect)
  ;;(aidermacs-default-model "openrouter/google/gemini-2.5-pro-exp-03-25:free")
  ;;(aidermacs-weak-model "openrouter/deepseek/deepseek-chat")
  (aidermacs-default-model "ollama_chat/qwen3-coder:latest")
  ;;(aidermacs-weak-model "ollama_chat/deepseek-r1:latest")
  ;;(aidermacs-default-model "openai/gpt-5.2")
  ;;(aidermacs-default-model "openai/gpt-5.2-chat-latest")
  ;;(aidermacs-weak-model "ollama_chat/deepseek-r1:latest")
  :config
  (setq aidermacs-program (expand-file-name "~/.local/bin/aider"))
  (add-to-list 'aidermacs-extra-args "--dark-mode")
  (add-hook 'aidermacs-before-run-backend-hook
            (lambda ()
              (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434")))
  (add-to-list 'display-buffer-alist
               `("\\*aidermacs.*\\*"
                 (display-buffer-pop-up-window)))
  :bind
  (("C-<tab>" . aidermacs-transient-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package gptel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-ai-agent)

;;; init-ai-agent.el ends here
