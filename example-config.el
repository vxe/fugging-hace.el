;;; example-config.el --- Example configuration for HuggingFace Browser -*- lexical-binding: t; -*-

;; This file shows example configurations for huggingface-browser.el
;; Copy relevant sections to your ~/.emacs or ~/.emacs.d/init.el

;;; Commentary:

;; Three configuration approaches:
;; 1. Minimal setup (browser only)
;; 2. Full setup with chatgpt-shell integration
;; 3. Advanced setup with custom filters and security

;;; Code:

;;;; ============================================================================
;;;; Approach 1: Minimal Setup (Browser Only)
;;;; ============================================================================
;;;; Use this if you just want to browse HuggingFace models
;;;; without full chat integration

(add-to-list 'load-path "/path/to/fugging-hace.el")
(require 'huggingface-browser)

;; Optional: Set API key for accessing models
(setq huggingface-browser-api-key "hf_your_token_here")

;; Optional: Customize default filters
(setq huggingface-browser-default-filters
      '((task . "text-generation")
        (sort . "downloads")
        (direction . -1)
        (limit . 100)))

;; Launch with: M-x huggingface-browser


;;;; ============================================================================
;;;; Approach 2: Full Setup with chatgpt-shell Integration
;;;; ============================================================================
;;;; Use this for complete functionality including chat sessions

;; Install chatgpt-shell first (via MELPA or straight)
;; M-x package-install RET chatgpt-shell RET

;; Load chatgpt-shell and dependencies
(require 'chatgpt-shell)
(require 'shell-maker)

;; Add HuggingFace browser to load path
(add-to-list 'load-path "/path/to/fugging-hace.el")

;; Load HuggingFace components
(require 'huggingface-browser)
(require 'chatgpt-shell-huggingface)

;; Set your HuggingFace API token
(setq chatgpt-shell-huggingface-key "hf_your_token_here")

;; Optional: Set a default HuggingFace model
(setq chatgpt-shell-model-version "meta-llama/Meta-Llama-3-8B-Instruct")

;; Optional: Enable streaming (recommended)
(setq chatgpt-shell-streaming t)

;; Optional: Keybindings
(global-set-key (kbd "C-c h b") #'huggingface-browser)
(global-set-key (kbd "C-c h s") #'chatgpt-shell)


;;;; ============================================================================
;;;; Approach 3: Advanced Setup with use-package
;;;; ============================================================================
;;;; Use this if you use use-package for configuration management

;; Install chatgpt-shell via use-package
(use-package chatgpt-shell
  :ensure t
  :custom
  (chatgpt-shell-streaming t)
  (chatgpt-shell-model-temperature 0.7))

;; Load HuggingFace browser
(use-package huggingface-browser
  :load-path "/path/to/fugging-hace.el"
  :after chatgpt-shell
  :custom
  ;; API configuration
  (huggingface-browser-api-key
   (lambda ()
     ;; Secure way: fetch from auth-source
     (auth-source-pass-get 'secret "huggingface-token")))

  ;; Default filters - show conversational models
  (huggingface-browser-default-filters
   '((task . "conversational")
     (sort . "downloads")
     (direction . -1)
     (limit . 50)))

  ;; Cache for 2 hours
  (huggingface-browser-cache-ttl 7200)

  :bind
  (("C-c h b" . huggingface-browser)
   ("C-c h f" . huggingface-browser-set-filter))

  :config
  ;; Load the chatgpt-shell provider
  (require 'chatgpt-shell-huggingface)

  ;; Set the same API key for the provider
  (setq chatgpt-shell-huggingface-key huggingface-browser-api-key)

  ;; Set a default HuggingFace model
  (setq chatgpt-shell-model-version "mistralai/Mistral-7B-Instruct-v0.2"))


;;;; ============================================================================
;;;; Secure API Key Management
;;;; ============================================================================
;;;; Different ways to securely store your HuggingFace API token

;; Method 1: Environment variable
(setq chatgpt-shell-huggingface-key
      (getenv "HUGGINGFACE_API_KEY"))

;; Method 2: Using pass (password-store)
(setq chatgpt-shell-huggingface-key
      (lambda ()
        (string-trim
         (shell-command-to-string "pass show huggingface/api-key"))))

;; Method 3: Using auth-source (recommended)
;; Add to ~/.authinfo or ~/.authinfo.gpg:
;;   machine api-inference.huggingface.co password hf_your_token_here
(setq chatgpt-shell-huggingface-key
      (lambda ()
        (auth-source-pick-first-password
         :host "api-inference.huggingface.co")))

;; Method 4: Using auth-source with pass backend
(require 'auth-source-pass)
(auth-source-pass-enable)
(setq chatgpt-shell-huggingface-key
      (lambda ()
        (auth-source-pass-get 'secret "huggingface-api-key")))


;;;; ============================================================================
;;;; Custom Filters and Workflows
;;;; ============================================================================

;; Function to quickly browse code generation models
(defun my/browse-code-models ()
  "Browse HuggingFace code generation models."
  (interactive)
  (let ((huggingface-browser-default-filters
         '((task . "text-generation")
           (search . "code")
           (sort . "downloads")
           (limit . 30))))
    (huggingface-browser)))

;; Function to browse summarization models
(defun my/browse-summary-models ()
  "Browse HuggingFace summarization models."
  (interactive)
  (let ((huggingface-browser-default-filters
         '((task . "summarization")
           (sort . "likes")
           (limit . 25))))
    (huggingface-browser)))

;; Add keybindings for custom workflows
(global-set-key (kbd "C-c h c") #'my/browse-code-models)
(global-set-key (kbd "C-c h s") #'my/browse-summary-models)


;;;; ============================================================================
;;;; Integration with Other Packages
;;;; ============================================================================

;; Integration with org-mode
(use-package ob-chatgpt-shell
  :ensure t
  :after (org chatgpt-shell)
  :config
  ;; Enable HuggingFace models in org-babel
  (ob-chatgpt-shell-setup))

;; Example org-babel block:
;; #+begin_src chatgpt-shell :version "meta-llama/Meta-Llama-3-8B-Instruct"
;; What is functional programming?
;; #+end_src


;;;; ============================================================================
;;;; Hooks and Customizations
;;;; ============================================================================

;; Add a hook to automatically set system prompts
(defun my/huggingface-chat-setup ()
  "Setup function for HuggingFace chat sessions."
  (when (and (boundp 'chatgpt-shell-model-version)
             (string-prefix-p "meta-llama" chatgpt-shell-model-version))
    (setq-local chatgpt-shell-system-prompt
                "You are a helpful, respectful and honest assistant.")))

(add-hook 'chatgpt-shell-mode-hook #'my/huggingface-chat-setup)


;;;; ============================================================================
;;;; Performance Tuning
;;;; ============================================================================

;; Increase cache TTL for slower connections
(setq huggingface-browser-cache-ttl 10800)  ; 3 hours

;; Reduce default model limit for faster loading
(setq huggingface-browser-default-filters
      '((task . "text-generation")
        (sort . "downloads")
        (limit . 50)))  ; Fetch only top 50

;; Disable streaming if you have connection issues
(setq chatgpt-shell-streaming nil)


;;;; ============================================================================
;;;; Multi-Provider Setup (HuggingFace + OpenAI + Anthropic)
;;;; ============================================================================

(use-package chatgpt-shell
  :ensure t
  :custom
  ;; OpenAI
  (chatgpt-shell-openai-key
   (lambda () (auth-source-pass-get 'secret "openai-key")))

  ;; Anthropic
  (chatgpt-shell-anthropic-key
   (lambda () (auth-source-pass-get 'secret "anthropic-key")))

  ;; HuggingFace
  (chatgpt-shell-huggingface-key
   (lambda () (auth-source-pass-get 'secret "huggingface-key")))

  ;; Default to HuggingFace Llama 3
  (chatgpt-shell-model-version "meta-llama/Meta-Llama-3-8B-Instruct")

  :config
  (require 'chatgpt-shell-huggingface)

  ;; Now you can swap between providers with:
  ;; M-x chatgpt-shell-swap-model
  )


;;;; ============================================================================
;;;; Debugging and Logging
;;;; ============================================================================

;; Enable detailed logging for troubleshooting
(setq chatgpt-shell-logging t)

;; Check the *chatgpt-log* buffer for API request/response details

;; Test API connectivity
(defun my/test-huggingface-api ()
  "Test HuggingFace API connectivity."
  (interactive)
  (require 'url)
  (let ((url-request-extra-headers
         `(("Authorization" .
            ,(concat "Bearer " (chatgpt-shell-huggingface--get-key))))))
    (url-retrieve
     "https://huggingface.co/api/models?limit=1"
     (lambda (status)
       (if (plist-get status :error)
           (message "API test failed: %s" (plist-get status :error))
         (message "API test successful!"))))))


;;; example-config.el ends here
