;;; chatgpt-shell-huggingface.el --- HuggingFace provider for chatgpt-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Keywords: ai, tools, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (chatgpt-shell "1.0"))

;;; Commentary:

;; HuggingFace Inference API provider for chatgpt-shell.
;; Enables chatgpt-shell to interact with HuggingFace models.
;;
;; Usage:
;;   (require 'chatgpt-shell-huggingface)
;;   (setq chatgpt-shell-huggingface-key "your-hf-token")
;;   ;; Models will be automatically available in chatgpt-shell
;;   M-x chatgpt-shell-swap-model RET
;;   ;; Select a HuggingFace model
;;
;; Or use the browser:
;;   M-x huggingface-browser RET
;;   ;; Select a model and press RET to launch chat

;;; Code:

(require 'cl-lib)
(require 'let-alist)
(require 'shell-maker nil t)

;;; Customization

(defgroup chatgpt-shell-huggingface nil
  "HuggingFace Inference API support for chatgpt-shell."
  :group 'chatgpt-shell
  :prefix "chatgpt-shell-huggingface-")

(defcustom chatgpt-shell-huggingface-key nil
  "HuggingFace API key (token) as a string or a function that loads and returns it."
  :type '(choice (string :tag "API Key")
                 (function :tag "Function returning API Key"))
  :group 'chatgpt-shell-huggingface)

(defcustom chatgpt-shell-huggingface-api-url-base "https://api-inference.huggingface.co"
  "HuggingFace Inference API's base URL."
  :type 'string
  :group 'chatgpt-shell-huggingface)

;;; Helper Functions

(defun chatgpt-shell-huggingface--get-key ()
  "Get the HuggingFace API key."
  (cond
   ((stringp chatgpt-shell-huggingface-key)
    chatgpt-shell-huggingface-key)
   ((functionp chatgpt-shell-huggingface-key)
    (funcall chatgpt-shell-huggingface-key))
   (t
    (error "HuggingFace API key not set. Set `chatgpt-shell-huggingface-key'"))))

;;; Model Factory

(cl-defun chatgpt-shell-huggingface--make-model (&key version
                                                      short-version
                                                      token-width
                                                      context-window
                                                      max-new-tokens
                                                      description)
  "Create a HuggingFace model definition.

Parameters:
  VERSION: Full model ID (e.g., 'meta-llama/Llama-2-7b-chat-hf')
  SHORT-VERSION: Display name (e.g., 'Llama-2-7b-chat')
  TOKEN-WIDTH: Average token width in characters
  CONTEXT-WINDOW: Maximum context length in tokens
  MAX-NEW-TOKENS: Maximum tokens to generate in response
  DESCRIPTION: Brief description of the model"
  `((:provider . "HuggingFace")
    (:label . "HF")
    (:version . ,version)
    (:short-version . ,(or short-version version))
    (:token-width . ,(or token-width 4))
    (:context-window . ,(or context-window 2048))
    (:max-new-tokens . ,(or max-new-tokens 512))
    (:description . ,(or description ""))
    (:handler . chatgpt-shell-huggingface--handle-command)
    (:filter . chatgpt-shell-huggingface--extract-response)
    (:payload . chatgpt-shell-huggingface--make-payload)
    (:url . chatgpt-shell-huggingface--make-url)
    (:headers . chatgpt-shell-huggingface--make-headers)
    (:url-base . chatgpt-shell-huggingface-api-url-base)
    (:key . chatgpt-shell-huggingface-key)
    (:validate-command . chatgpt-shell-huggingface--validate-command)))

;;; Default Models

(defun chatgpt-shell-huggingface-models ()
  "Build a list of popular HuggingFace LLM models.
These are well-known models available via the Inference API."
  (list
   ;; Meta Llama models
   (chatgpt-shell-huggingface--make-model
    :version "meta-llama/Meta-Llama-3-8B-Instruct"
    :short-version "Llama-3-8B"
    :token-width 4
    :context-window 8192
    :max-new-tokens 2048
    :description "Meta's Llama 3 8B instruction-tuned model")

   (chatgpt-shell-huggingface--make-model
    :version "meta-llama/Llama-2-7b-chat-hf"
    :short-version "Llama-2-7b-chat"
    :token-width 4
    :context-window 4096
    :max-new-tokens 1024
    :description "Meta's Llama 2 7B chat model")

   ;; Mistral models
   (chatgpt-shell-huggingface--make-model
    :version "mistralai/Mistral-7B-Instruct-v0.2"
    :short-version "Mistral-7B-v0.2"
    :token-width 4
    :context-window 8192
    :max-new-tokens 2048
    :description "Mistral AI's 7B instruction model v0.2")

   (chatgpt-shell-huggingface--make-model
    :version "mistralai/Mixtral-8x7B-Instruct-v0.1"
    :short-version "Mixtral-8x7B"
    :token-width 4
    :context-window 32768
    :max-new-tokens 4096
    :description "Mistral AI's Mixtral 8x7B MoE model")

   ;; Google models
   (chatgpt-shell-huggingface--make-model
    :version "google/flan-t5-xxl"
    :short-version "FLAN-T5-XXL"
    :token-width 4
    :context-window 512
    :max-new-tokens 512
    :description "Google's FLAN-T5 XXL model")

   ;; Microsoft models
   (chatgpt-shell-huggingface--make-model
    :version "microsoft/DialoGPT-large"
    :short-version "DialoGPT-large"
    :token-width 4
    :context-window 1024
    :max-new-tokens 512
    :description "Microsoft's conversational model")

   ;; Falcon models
   (chatgpt-shell-huggingface--make-model
    :version "tiiuae/falcon-7b-instruct"
    :short-version "Falcon-7B-instruct"
    :token-width 4
    :context-window 2048
    :max-new-tokens 1024
    :description "TII's Falcon 7B instruction model")

   ;; MPT models
   (chatgpt-shell-huggingface--make-model
    :version "mosaicml/mpt-7b-chat"
    :short-version "MPT-7B-chat"
    :token-width 4
    :context-window 2048
    :max-new-tokens 1024
    :description "MosaicML's MPT 7B chat model")))

;;; Validation

(cl-defun chatgpt-shell-huggingface--validate-command (&key model command)
  "Validate COMMAND for MODEL before sending to HuggingFace API.
Returns nil if valid, or an error message string if invalid."
  (cond
   ((string-empty-p command)
    "Command cannot be empty")
   ((not (chatgpt-shell-huggingface--get-key))
    "HuggingFace API key not set. Set `chatgpt-shell-huggingface-key'")
   (t nil)))

;;; URL Construction

(cl-defun chatgpt-shell-huggingface--make-url (&key model settings)
  "Build the API URL for MODEL with SETTINGS."
  (let ((base-url (or (map-elt model :url-base)
                      chatgpt-shell-huggingface-api-url-base))
        (model-id (map-elt model :version)))
    (format "%s/models/%s" base-url model-id)))

;;; Headers

(cl-defun chatgpt-shell-huggingface--make-headers ()
  "Create HTTP headers for HuggingFace API request."
  (list (cons "Content-Type" "application/json")
        (cons "Authorization" (format "Bearer %s"
                                      (chatgpt-shell-huggingface--get-key)))))

;;; Payload Construction

(cl-defun chatgpt-shell-huggingface--make-payload (&key model context settings)
  "Create the API payload for MODEL using CONTEXT and SETTINGS.

CONTEXT is a list of (prompt . response) pairs representing conversation history.
The current prompt is the last element in CONTEXT with a nil response."
  (let* ((messages (chatgpt-shell-huggingface--format-context context))
         (max-new-tokens (or (map-elt model :max-new-tokens) 512))
         (temperature (map-elt settings :temperature))
         (system-prompt (map-elt settings :system-prompt))
         (inputs (if system-prompt
                     (format "%s\n\n%s" system-prompt messages)
                   messages))
         (parameters (list)))

    ;; Build parameters
    (push (cons 'max_new_tokens max-new-tokens) parameters)
    (when temperature
      (push (cons 'temperature temperature) parameters))

    ;; Add common generation parameters
    (push (cons 'return_full_text :false) parameters)
    (push (cons 'do_sample t) parameters)

    ;; Return payload
    (list (cons 'inputs inputs)
          (cons 'parameters parameters))))

(defun chatgpt-shell-huggingface--format-context (context)
  "Format CONTEXT (list of prompt/response pairs) into a single string.
This creates a conversation-style prompt for the model."
  (let ((formatted ""))
    (dolist (interaction context)
      (let ((prompt (car interaction))
            (response (cdr interaction)))
        (setq formatted (concat formatted
                                (format "User: %s\n" prompt)))
        (when response
          (setq formatted (concat formatted
                                  (format "Assistant: %s\n" response))))))
    ;; For the last prompt (current one), add "Assistant:" to prompt a response
    (concat formatted "Assistant:")))

;;; Request Handler

(cl-defun chatgpt-shell-huggingface--handle-command (&key model command context shell settings)
  "Handle HuggingFace shell COMMAND using MODEL, CONTEXT, SHELL, and SETTINGS."
  (when-let ((error-msg (chatgpt-shell-huggingface--validate-command
                         :model model :command command)))
    (error error-msg))

  (shell-maker-make-http-request
   :async t
   :url (chatgpt-shell-huggingface--make-url :model model :settings settings)
   :data (chatgpt-shell-huggingface--make-payload
          :model model
          :context (append context (list (cons command nil)))
          :settings settings)
   :headers (chatgpt-shell-huggingface--make-headers)
   :filter #'chatgpt-shell-huggingface--extract-response
   :shell shell))

;;; Response Extraction

(defun chatgpt-shell-huggingface--extract-response (output)
  "Extract response text from HuggingFace API OUTPUT.

OUTPUT format from shell-maker is:
  ((:function-calls . ...)
   (:pending . ...)
   (:filtered . ...))

This function processes the pending JSON response and extracts the generated text."
  (when (stringp output)
    (error "Please upgrade shell-maker to 0.79.1 or newer"))

  (let ((pending (map-elt output :pending)))
    (if (string-empty-p pending)
        output
      (condition-case err
          (let* ((json-object-type 'plist)
                 (json-array-type 'list)
                 (json-key-type 'keyword)
                 (parsed (json-read-from-string pending)))

            ;; HuggingFace Inference API returns different formats:
            ;; 1. Array of results: [{"generated_text": "..."}]
            ;; 2. Single result: {"generated_text": "..."}
            ;; 3. Error: {"error": "..."}

            (cond
             ;; Handle error response
             ((plist-get parsed :error)
              (list (cons :filtered
                          (format "Error: %s" (plist-get parsed :error)))
                    (cons :pending "")))

             ;; Handle array of results
             ((listp parsed)
              (let* ((first-result (car parsed))
                     (generated-text (plist-get first-result :generated_text)))
                (if generated-text
                    (list (cons :filtered generated-text)
                          (cons :pending ""))
                  output)))

             ;; Handle single result object
             ((plist-get parsed :generated_text)
              (list (cons :filtered (plist-get parsed :generated_text))
                    (cons :pending "")))

             ;; Unknown format
             (t output)))

        ;; If JSON parsing fails, return unchanged
        (error
         (message "HuggingFace response parse error: %s" (error-message-string err))
         output)))))

;;; Dynamic Model Loading

(defun chatgpt-shell-huggingface--create-model-from-id (model-id)
  "Create a model definition from MODEL-ID string.
This is a simplified version for dynamically selected models."
  (let* ((name-parts (split-string model-id "/"))
         (short-name (if (= (length name-parts) 2)
                         (cadr name-parts)
                       model-id)))
    (chatgpt-shell-huggingface--make-model
     :version model-id
     :short-version short-name
     :token-width 4
     :context-window 2048
     :max-new-tokens 512
     :description (format "HuggingFace model: %s" model-id))))

(defun chatgpt-shell-huggingface-add-model (model-id)
  "Add a HuggingFace MODEL-ID to chatgpt-shell's available models.
Creates a model definition and adds it to `chatgpt-shell-models'."
  (interactive "sHuggingFace Model ID: ")
  (if (not (boundp 'chatgpt-shell-models))
      (error "chatgpt-shell not loaded. Please (require 'chatgpt-shell) first")
    (let ((new-model (chatgpt-shell-huggingface--create-model-from-id model-id)))
      ;; Check if model already exists
      (if (seq-find (lambda (m)
                      (string= (map-elt m :version) model-id))
                    chatgpt-shell-models)
          (message "HuggingFace model already available: %s" model-id)
        ;; Add to the list
        (push new-model chatgpt-shell-models)
        (message "Added HuggingFace model: %s" model-id)
        new-model))))

;;; Integration with Browser

(defun chatgpt-shell-huggingface-start (model-id)
  "Start a chatgpt-shell session with HuggingFace MODEL-ID.
This is called from huggingface-browser when selecting a model."
  (interactive "sModel ID: ")

  ;; Check if chatgpt-shell is available
  (unless (fboundp 'chatgpt-shell)
    (error "chatgpt-shell not available. Please install and load chatgpt-shell package"))

  ;; Ensure the model is available
  (let ((model (chatgpt-shell-huggingface-add-model model-id)))
    (unless model
      (error "Failed to add model: %s" model-id)))

  ;; Set as active model and start shell
  (setq chatgpt-shell-model-version model-id)
  (message "Starting HuggingFace shell with: %s" model-id)

  (let ((buffer (chatgpt-shell)))
    ;; Show confirmation message
    (message "🤗 HuggingFace shell started with model: %s" model-id)
    ;; Add a welcome message to the buffer
    (with-current-buffer buffer
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (propertize
                 (format "\n[Using HuggingFace model: %s]\n" model-id)
                 'face 'font-lock-comment-face))
        (insert (propertize
                 (format "[API: %s/models/%s]\n\n"
                         chatgpt-shell-huggingface-api-url-base
                         model-id)
                 'face 'font-lock-comment-face))))
    buffer))

;;; Verification and Debugging Commands

(defun chatgpt-shell-huggingface-debug-model (model-id)
  "Debug why MODEL-ID might not be working.
Shows whether chatgpt-shell is loaded, if the model exists, and other diagnostics."
  (interactive
   (list (or (and (boundp 'chatgpt-shell-model-version)
                  chatgpt-shell-model-version)
             (read-string "Model ID to debug: "))))

  (with-current-buffer (get-buffer-create "*HuggingFace Debug*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "🔍 HuggingFace Model Debug Info\n" 'face 'bold))
      (insert (make-string 50 ?=) "\n\n")

      ;; Check chatgpt-shell
      (insert (propertize "1. chatgpt-shell Status:\n" 'face 'bold))
      (if (fboundp 'chatgpt-shell)
          (insert "   ✅ chatgpt-shell function is available\n")
        (insert "   ❌ chatgpt-shell function NOT found\n"
                "   → Install chatgpt-shell: M-x package-install RET chatgpt-shell\n"))

      (if (boundp 'chatgpt-shell-models)
          (insert (format "   ✅ chatgpt-shell-models variable exists (%d models)\n"
                          (length chatgpt-shell-models)))
        (insert "   ❌ chatgpt-shell-models variable NOT found\n"
                "   → Load chatgpt-shell: (require 'chatgpt-shell)\n"))

      (insert "\n")

      ;; Check model existence
      (insert (propertize "2. Model Status:\n" 'face 'bold))
      (insert (format "   Looking for: %s\n" model-id))

      (if (not (boundp 'chatgpt-shell-models))
          (insert "   ⊘ Cannot check - chatgpt-shell-models not loaded\n")
        (let ((model (seq-find (lambda (m)
                                 (string= (map-elt m :version) model-id))
                               chatgpt-shell-models)))
          (if model
              (progn
                (insert "   ✅ Model found in chatgpt-shell-models\n")
                (insert (format "      Provider: %s\n" (map-elt model :provider)))
                (insert (format "      Short name: %s\n" (map-elt model :short-version))))
            (insert "   ❌ Model NOT found in chatgpt-shell-models\n")
            (insert "   → Add it: (chatgpt-shell-huggingface-add-model \"" model-id "\")\n"))))

      (insert "\n")

      ;; Check current model setting
      (insert (propertize "3. Current Settings:\n" 'face 'bold))
      (if (boundp 'chatgpt-shell-model-version)
          (insert (format "   chatgpt-shell-model-version: %s\n"
                          chatgpt-shell-model-version))
        (insert "   chatgpt-shell-model-version: NOT SET\n"))

      (insert "\n")

      ;; Check HuggingFace models
      (insert (propertize "4. Available HuggingFace Models:\n" 'face 'bold))
      (if (not (boundp 'chatgpt-shell-models))
          (insert "   ⊘ Cannot check - chatgpt-shell-models not loaded\n")
        (let ((hf-models (seq-filter
                          (lambda (m)
                            (string= (map-elt m :provider) "HuggingFace"))
                          chatgpt-shell-models)))
          (if hf-models
              (progn
                (insert (format "   Found %d HuggingFace models:\n" (length hf-models)))
                (dolist (m hf-models)
                  (insert (format "      - %s\n" (map-elt m :version)))))
            (insert "   ⊘ No HuggingFace models loaded\n")
            (insert "   → Load default models: (require 'chatgpt-shell-huggingface)\n"))))

      (insert "\n")

      ;; API key check
      (insert (propertize "5. API Key Status:\n" 'face 'bold))
      (condition-case err
          (let ((key (chatgpt-shell-huggingface--get-key)))
            (if (and key (> (length key) 0))
                (insert (format "   ✅ API key is set (length: %d)\n" (length key)))
              (insert "   ❌ API key is empty or nil\n")))
        (error
         (insert (format "   ❌ Error getting API key: %s\n" (error-message-string err)))))

      (insert "\n")
      (insert (propertize "Press 'q' to close.\n" 'face 'italic))
      (goto-char (point-min)))
    (view-mode)
    (pop-to-buffer (current-buffer))))

(defun chatgpt-shell-huggingface-current-model ()
  "Show information about the currently active HuggingFace model."
  (interactive)
  (if (and (boundp 'chatgpt-shell-model-version)
           chatgpt-shell-model-version)
      (let* ((model-id chatgpt-shell-model-version)
             (model (seq-find (lambda (m)
                                (string= (map-elt m :version) model-id))
                              chatgpt-shell-models))
             (provider (when model (map-elt model :provider))))
        (if (string= provider "HuggingFace")
            (progn
              (message "Current model: %s (HuggingFace)\nAPI URL: %s/models/%s"
                       model-id
                       chatgpt-shell-huggingface-api-url-base
                       model-id)
              ;; Show detailed info in a popup
              (with-current-buffer (get-buffer-create "*HuggingFace Model Info*")
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert (propertize "🤗 HuggingFace Model Information\n" 'face 'bold))
                  (insert (make-string 50 ?=) "\n\n")
                  (insert (propertize "Model ID: " 'face 'bold) model-id "\n")
                  (insert (propertize "Provider: " 'face 'bold) "HuggingFace\n")
                  (insert (propertize "Short Name: " 'face 'bold)
                          (or (map-elt model :short-version) model-id) "\n")
                  (insert (propertize "API Endpoint: " 'face 'bold)
                          (format "%s/models/%s\n"
                                  chatgpt-shell-huggingface-api-url-base
                                  model-id))
                  (insert (propertize "Context Window: " 'face 'bold)
                          (format "%s tokens\n" (map-elt model :context-window)))
                  (insert (propertize "Max Output: " 'face 'bold)
                          (format "%s tokens\n" (map-elt model :max-new-tokens)))
                  (insert (propertize "Token Width: " 'face 'bold)
                          (format "~%s chars/token\n" (map-elt model :token-width)))
                  (when-let ((desc (map-elt model :description)))
                    (insert "\n" (propertize "Description:\n" 'face 'bold))
                    (insert desc "\n"))
                  (insert "\n" (propertize "Press 'q' to close.\n" 'face 'italic))
                  (goto-char (point-min)))
                (view-mode))
              (pop-to-buffer "*HuggingFace Model Info*"))
          (message "Current model is NOT a HuggingFace model: %s (%s)"
                   model-id provider)))
    (message "No model currently active")))

(defun chatgpt-shell-huggingface-verify-api ()
  "Verify that the HuggingFace API is accessible and token is valid."
  (interactive)
  (message "Testing HuggingFace API connection...")
  (let ((url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer " (chatgpt-shell-huggingface--get-key))))))
    (url-retrieve
     "https://huggingface.co/api/whoami-v2"
     (lambda (status)
       (if (plist-get status :error)
           (message "❌ API Error: %s\nCheck your token in chatgpt-shell-huggingface-key"
                    (plist-get status :error))
         (goto-char (point-min))
         (re-search-forward "^$")
         (let* ((json-object-type 'plist)
                (json-array-type 'list)
                (json-key-type 'keyword)
                (data (json-read))
                (username (plist-get data :name))
                (auth-type (plist-get data :type)))
           (message "✅ HuggingFace API is working!\nAuthenticated as: %s (%s)"
                    username auth-type)
           (kill-buffer)))))))

(defun chatgpt-shell-huggingface-list-models ()
  "List all available HuggingFace models in chatgpt-shell."
  (interactive)
  (let ((hf-models (seq-filter
                    (lambda (m)
                      (string= (map-elt m :provider) "HuggingFace"))
                    chatgpt-shell-models)))
    (if hf-models
        (with-current-buffer (get-buffer-create "*HuggingFace Models*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize "🤗 Available HuggingFace Models\n" 'face 'bold))
            (insert (make-string 50 ?=) "\n\n")
            (insert (format "Total: %d models\n\n" (length hf-models)))
            (dolist (model hf-models)
              (let ((id (map-elt model :version))
                    (short (map-elt model :short-version))
                    (desc (map-elt model :description))
                    (active (and (boundp 'chatgpt-shell-model-version)
                                 (string= chatgpt-shell-model-version
                                          (map-elt model :version)))))
                (insert (propertize
                         (if active "▸ " "  ")
                         'face (if active 'success 'default)))
                (insert (propertize short 'face 'font-lock-function-name-face))
                (insert "\n  " id "\n")
                (when desc
                  (insert "  " (propertize desc 'face 'font-lock-comment-face) "\n"))
                (insert "\n")))
            (insert (propertize "\n▸ = currently active\n" 'face 'italic))
            (insert (propertize "Press 'q' to close.\n" 'face 'italic))
            (goto-char (point-min)))
          (view-mode)
          (pop-to-buffer (current-buffer)))
      (message "No HuggingFace models available. Run M-x huggingface-browser to add some."))))

(defun chatgpt-shell-huggingface-show-request-log ()
  "Show the last request sent to HuggingFace API (requires chatgpt-shell-logging)."
  (interactive)
  (if (and (boundp 'chatgpt-shell-logging)
           chatgpt-shell-logging)
      (if (get-buffer "*chatgpt-log*")
          (progn
            (pop-to-buffer "*chatgpt-log*")
            (message "Showing API request log. Look for HuggingFace URLs."))
        (message "No log buffer found. Make sure you've sent at least one request."))
    (message "Enable logging first: (setq chatgpt-shell-logging t)")))

(provide 'chatgpt-shell-huggingface)

;;; chatgpt-shell-huggingface.el ends here
