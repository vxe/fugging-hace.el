;;; huggingface-browser.el --- Browse and select HuggingFace models -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Keywords: ai, tools, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (chatgpt-shell "1.0"))

;;; Commentary:

;; This package provides an interactive browser for HuggingFace models
;; using tabulated-list-mode. Users can browse available models,
;; view details, and launch chat sessions directly from the browser.
;;
;; Usage:
;;   M-x huggingface-browser RET
;;   - Navigate with n/p or arrow keys
;;   - Press RET to launch chat with selected model
;;   - Press d to view model details
;;   - Press r to refresh model list
;;   - Press q to quit

;;; Code:

(require 'json)
(require 'url)
(require 'tabulated-list)
(require 'chatgpt-shell nil t) ;; Optional dependency

;;; Customization

(defgroup huggingface-browser nil
  "Browse and interact with HuggingFace models."
  :group 'external
  :prefix "huggingface-browser-")

(defcustom huggingface-browser-api-url "https://huggingface.co/api/models"
  "HuggingFace Hub API URL for fetching models."
  :type 'string
  :group 'huggingface-browser)

(defcustom huggingface-browser-api-key nil
  "HuggingFace API key (optional for inference API).
Can be a string or a function that returns the key."
  :type '(choice (string :tag "API Key")
                 (function :tag "Function returning API Key"))
  :group 'huggingface-browser)

(defcustom huggingface-browser-inference-url "https://api-inference.huggingface.co/models"
  "Base URL for HuggingFace Inference API."
  :type 'string
  :group 'huggingface-browser)

(defcustom huggingface-browser-default-filters
  '((task . "text-generation")
    (sort . "downloads")
    (direction . -1)
    (limit . 100))
  "Default filters for fetching models from HuggingFace Hub.
Alist with keys:
  - task: Filter by task (e.g., 'text-generation', 'conversational')
  - sort: Sort by field (e.g., 'downloads', 'likes', 'trending')
  - direction: Sort direction (1 for ascending, -1 for descending)
  - limit: Maximum number of models to fetch"
  :type '(alist :key-type symbol :value-type sexp)
  :group 'huggingface-browser)

(defcustom huggingface-browser-cache-ttl 3600
  "Time-to-live for cached model data in seconds (default: 1 hour)."
  :type 'integer
  :group 'huggingface-browser)

;;; Internal Variables

(defvar huggingface-browser--models-cache nil
  "Cache of fetched models. List of plists with model data.")

(defvar huggingface-browser--cache-timestamp nil
  "Timestamp when models cache was last updated.")

(defvar huggingface-browser--selected-model nil
  "Currently selected model in the browser.")

;;; API Functions

(defun huggingface-browser--get-api-key ()
  "Get the HuggingFace API key from variable or function."
  (cond
   ((stringp huggingface-browser-api-key)
    huggingface-browser-api-key)
   ((functionp huggingface-browser-api-key)
    (funcall huggingface-browser-api-key))
   (t nil)))

(defun huggingface-browser--build-api-url (&optional filters)
  "Build API URL with FILTERS (alist of query parameters)."
  (let* ((base-url huggingface-browser-api-url)
         (filter-params (or filters huggingface-browser-default-filters))
         (query-string
          (mapconcat
           (lambda (param)
             (let ((key (car param))
                   (value (cdr param)))
               (format "%s=%s"
                       (url-hexify-string (symbol-name key))
                       (url-hexify-string (format "%s" value)))))
           filter-params
           "&")))
    (if (string-empty-p query-string)
        base-url
      (concat base-url "?" query-string))))

(defun huggingface-browser--fetch-models-sync (&optional filters)
  "Fetch models synchronously from HuggingFace API with optional FILTERS.
Returns parsed JSON as a list of plists."
  (let* ((url (huggingface-browser--build-api-url filters))
         (url-request-extra-headers
          (when-let ((key (huggingface-browser--get-api-key)))
            `(("Authorization" . ,(concat "Bearer " key)))))
         (buffer (url-retrieve-synchronously url t nil 30)))
    (unless buffer
      (error "Failed to fetch models from HuggingFace API"))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "^$") ;; Skip HTTP headers
      (let* ((json-object-type 'plist)
             (json-array-type 'list)
             (json-key-type 'keyword)
             (data (json-read)))
        (kill-buffer)
        data))))

(defun huggingface-browser--fetch-models-async (callback &optional filters)
  "Fetch models asynchronously from HuggingFace API with optional FILTERS.
Call CALLBACK with parsed model list when complete."
  (let* ((url (huggingface-browser--build-api-url filters))
         (url-request-extra-headers
          (when-let ((key (huggingface-browser--get-api-key)))
            `(("Authorization" . ,(concat "Bearer " key))))))
    (url-retrieve
     url
     (lambda (status)
       (if (plist-get status :error)
           (error "Failed to fetch models: %s" (plist-get status :error))
         (goto-char (point-min))
         (re-search-forward "^$")
         (let* ((json-object-type 'plist)
                (json-array-type 'list)
                (json-key-type 'keyword)
                (data (json-read)))
           (kill-buffer)
           (funcall callback data))))
     nil t)))

(defun huggingface-browser--cache-valid-p ()
  "Check if model cache is still valid based on TTL."
  (and huggingface-browser--models-cache
       huggingface-browser--cache-timestamp
       (< (- (float-time) huggingface-browser--cache-timestamp)
          huggingface-browser-cache-ttl)))

(defun huggingface-browser--get-models (&optional force-refresh)
  "Get models from cache or fetch from API.
If FORCE-REFRESH is non-nil, always fetch fresh data."
  (if (and (not force-refresh)
           (huggingface-browser--cache-valid-p))
      huggingface-browser--models-cache
    (let ((models (huggingface-browser--fetch-models-sync)))
      (setq huggingface-browser--models-cache models
            huggingface-browser--cache-timestamp (float-time))
      models)))

;;; Model Data Processing

(defun huggingface-browser--extract-model-info (model)
  "Extract relevant information from MODEL plist.
Returns a plist with normalized fields."
  (let* ((model-id (plist-get model :id))
         (model-name (or (plist-get model :modelId) model-id))
         (tags (plist-get model :tags))
         (pipeline-tag (plist-get model :pipeline_tag))
         (downloads (or (plist-get model :downloads) 0))
         (likes (or (plist-get model :likes) 0))
         (author (or (plist-get model :author) ""))
         (description (or (plist-get model :description) ""))
         ;; Truncate description for display
         (short-desc (if (> (length description) 60)
                         (concat (substring description 0 57) "...")
                       description)))
    (list :id model-id
          :name model-name
          :type (or pipeline-tag "unknown")
          :tags (if (listp tags) (mapconcat #'identity tags ", ") "")
          :description short-desc
          :full-description description
          :downloads downloads
          :likes likes
          :author author
          :raw-data model)))

(defun huggingface-browser--format-number (num)
  "Format NUM with K/M/B suffixes for readability."
  (cond
   ((>= num 1000000000) (format "%.1fB" (/ num 1000000000.0)))
   ((>= num 1000000) (format "%.1fM" (/ num 1000000.0)))
   ((>= num 1000) (format "%.1fK" (/ num 1000.0)))
   (t (format "%d" num))))

;;; Tabulated List Mode Setup

(defvar huggingface-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'huggingface-browser-select-model)
    (define-key map (kbd "d") #'huggingface-browser-describe-model)
    (define-key map (kbd "r") #'huggingface-browser-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'huggingface-browser-refresh)
    (define-key map (kbd "f") #'huggingface-browser-set-filter)
    map)
  "Keymap for HuggingFace browser mode.")

(define-derived-mode huggingface-browser-mode tabulated-list-mode "HF-Browser"
  "Major mode for browsing HuggingFace models.

\\{huggingface-browser-mode-map}"
  (setq tabulated-list-format
        [("Model" 35 t)
         ("Type" 18 t)
         ("Downloads" 10 t :right-align t)
         ("Likes" 8 t :right-align t)
         ("Description" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Downloads" t))
  (tabulated-list-init-header))

(defun huggingface-browser--make-entry (model)
  "Create a tabulated-list entry for MODEL."
  (let* ((info (huggingface-browser--extract-model-info model))
         (id (plist-get info :id))
         (name (plist-get info :name))
         (type (plist-get info :type))
         (downloads (plist-get info :downloads))
         (likes (plist-get info :likes))
         (description (plist-get info :description)))
    (list info
          (vector
           (propertize name 'face 'font-lock-function-name-face)
           (propertize type 'face 'font-lock-type-face)
           (huggingface-browser--format-number downloads)
           (huggingface-browser--format-number likes)
           description))))

(defun huggingface-browser--populate-buffer (models)
  "Populate current buffer with MODELS data."
  (setq tabulated-list-entries
        (mapcar #'huggingface-browser--make-entry models))
  (tabulated-list-print t))

;;; Interactive Commands

;;;###autoload
(defun huggingface-browser (&optional force-refresh)
  "Open HuggingFace model browser.
With prefix argument FORCE-REFRESH, fetch fresh model data."
  (interactive "P")
  (let ((buffer (get-buffer-create "*HuggingFace Models*")))
    (with-current-buffer buffer
      (huggingface-browser-mode)
      (message "Fetching HuggingFace models...")
      (let ((models (huggingface-browser--get-models force-refresh)))
        (huggingface-browser--populate-buffer models)
        (message "Loaded %d models" (length models))))
    (switch-to-buffer buffer)))

(defun huggingface-browser-refresh ()
  "Refresh the model list from HuggingFace API."
  (interactive)
  (message "Refreshing model list...")
  (let ((models (huggingface-browser--get-models t)))
    (huggingface-browser--populate-buffer models)
    (message "Refreshed: %d models loaded" (length models))))

(defun huggingface-browser-select-model ()
  "Select the model at point and launch a chat session."
  (interactive)
  (let ((model-info (tabulated-list-get-id)))
    (unless model-info
      (error "No model selected"))
    (setq huggingface-browser--selected-model model-info)
    (let ((model-id (plist-get model-info :id)))
      (message "Selected model: %s" model-id)
      ;; Check if chatgpt-shell is available
      (if (fboundp 'chatgpt-shell)
          (huggingface-browser--launch-chat model-info)
        (huggingface-browser--launch-simple-chat model-info)))))

(defun huggingface-browser-describe-model ()
  "Show detailed information about the model at point."
  (interactive)
  (let ((model-info (tabulated-list-get-id)))
    (unless model-info
      (error "No model selected"))
    (let* ((model-id (plist-get model-info :id))
           (type (plist-get model-info :type))
           (tags (plist-get model-info :tags))
           (description (plist-get model-info :full-description))
           (downloads (plist-get model-info :downloads))
           (likes (plist-get model-info :likes))
           (author (plist-get model-info :author))
           (buffer (get-buffer-create "*HuggingFace Model Details*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "HuggingFace Model Details\n" 'face 'bold))
          (insert (propertize (make-string 50 ?=) 'face 'shadow) "\n\n")
          (insert (propertize "Model ID: " 'face 'bold) model-id "\n")
          (insert (propertize "Author: " 'face 'bold) author "\n")
          (insert (propertize "Type: " 'face 'bold) type "\n")
          (insert (propertize "Downloads: " 'face 'bold)
                  (format "%s\n" (huggingface-browser--format-number downloads)))
          (insert (propertize "Likes: " 'face 'bold)
                  (format "%s\n" (huggingface-browser--format-number likes)))
          (when (not (string-empty-p tags))
            (insert (propertize "Tags: " 'face 'bold) tags "\n"))
          (insert "\n" (propertize "Description:\n" 'face 'bold))
          (insert (or description "No description available.") "\n\n")
          (insert (propertize "Press 'q' to close this window.\n" 'face 'italic))
          (goto-char (point-min)))
        (view-mode))
      (pop-to-buffer buffer))))

(defun huggingface-browser-set-filter ()
  "Interactively set filters for model search."
  (interactive)
  (let* ((task (completing-read
                "Task (empty for all): "
                '("text-generation" "conversational" "text2text-generation"
                  "fill-mask" "token-classification" "question-answering"
                  "summarization" "translation")
                nil nil))
         (sort-by (completing-read
                   "Sort by: "
                   '("downloads" "likes" "trending" "createdAt" "lastModified")
                   nil t nil nil "downloads"))
         (limit (read-number "Limit (max models): " 100))
         (filters `((sort . ,sort-by)
                    (direction . -1)
                    (limit . ,limit))))
    (when (not (string-empty-p task))
      (push (cons 'task task) filters))
    (message "Fetching models with filters: %s" filters)
    (setq huggingface-browser--models-cache nil) ;; Clear cache
    (let ((models (huggingface-browser--fetch-models-sync filters)))
      (setq huggingface-browser--models-cache models
            huggingface-browser--cache-timestamp (float-time))
      (huggingface-browser--populate-buffer models)
      (message "Loaded %d models" (length models)))))

;;; Chat Integration

(defun huggingface-browser--launch-chat (model-info)
  "Launch a chatgpt-shell session with MODEL-INFO."
  (let ((model-id (plist-get model-info :id)))
    (message "Launching chat with %s..." model-id)
    ;; Try to load the HuggingFace provider if not already loaded
    (unless (fboundp 'chatgpt-shell-huggingface-start)
      (condition-case err
          (require 'chatgpt-shell-huggingface)
        (error
         (message "Could not load chatgpt-shell-huggingface: %s"
                  (error-message-string err)))))
    ;; Launch chat using the provider or fallback
    (if (fboundp 'chatgpt-shell-huggingface-start)
        (chatgpt-shell-huggingface-start model-id)
      (message "HuggingFace provider not available. Creating simple chat buffer...")
      (huggingface-browser--launch-simple-chat model-info))))

(defun huggingface-browser--launch-simple-chat (model-info)
  "Launch a simple chat buffer for MODEL-INFO (fallback when chatgpt-shell not available)."
  (let* ((model-id (plist-get model-info :id))
         (buffer-name (format "*HuggingFace Chat: %s*" model-id))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'comint-mode)
        (comint-mode))
      (setq-local huggingface-browser--current-model model-info)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize (format "\n=== Chat Session: %s ===\n" model-id)
                            'face 'bold))
        (insert (propertize "Note: Full chat integration requires chatgpt-shell.\n"
                            'face 'warning))
        (insert "This is a placeholder buffer.\n\n")
        (insert (propertize "Model: " 'face 'bold) model-id "\n")
        (insert (propertize "Type: " 'face 'bold)
                (plist-get model-info :type) "\n\n")
        (insert "To enable full functionality, load the HuggingFace provider.\n\n")))
    (pop-to-buffer buffer)
    (message "Opened chat buffer for %s" model-id)))

(provide 'huggingface-browser)

;;; huggingface-browser.el ends here
