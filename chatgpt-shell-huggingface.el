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
  (when (fboundp 'chatgpt-shell-models)
    (let ((new-model (chatgpt-shell-huggingface--create-model-from-id model-id)))
      ;; Check if model already exists
      (unless (seq-find (lambda (m)
                          (string= (map-elt m :version) model-id))
                        chatgpt-shell-models)
        ;; Add to the list
        (add-to-list 'chatgpt-shell-models new-model)
        (message "Added HuggingFace model: %s" model-id)))))

;;; Integration with Browser

(defun chatgpt-shell-huggingface-start (model-id)
  "Start a chatgpt-shell session with HuggingFace MODEL-ID.
This is called from huggingface-browser when selecting a model."
  (interactive "sModel ID: ")

  ;; Ensure the model is available
  (chatgpt-shell-huggingface-add-model model-id)

  ;; Set as active model
  (when (fboundp 'chatgpt-shell)
    (setq chatgpt-shell-model-version model-id)
    (chatgpt-shell)))

(provide 'chatgpt-shell-huggingface)

;;; chatgpt-shell-huggingface.el ends here
