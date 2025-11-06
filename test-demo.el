;;; test-demo.el --- Test and demo script for HuggingFace Browser -*- lexical-binding: t; -*-

;; This file provides test functions and demos to verify the HuggingFace Browser works correctly

;;; Commentary:

;; Run these tests to ensure everything is working:
;; 1. Load this file: M-x load-file RET test-demo.el RET
;; 2. Run individual tests with M-x <test-name>
;; 3. Or run all tests: M-x hf-test-run-all

;;; Code:

(require 'huggingface-browser nil t)
(require 'chatgpt-shell-huggingface nil t)

;;; Test 1: API Connectivity

(defun hf-test-api-connectivity ()
  "Test basic connectivity to HuggingFace API."
  (interactive)
  (message "Testing HuggingFace API connectivity...")
  (condition-case err
      (let ((url "https://huggingface.co/api/models?limit=1"))
        (with-current-buffer (url-retrieve-synchronously url t nil 10)
          (goto-char (point-min))
          (if (re-search-forward "HTTP/[0-9.]+ 200" nil t)
              (progn
                (message "✓ API connectivity test PASSED")
                (kill-buffer)
                t)
            (message "✗ API connectivity test FAILED: Non-200 response")
            (kill-buffer)
            nil)))
    (error
     (message "✗ API connectivity test FAILED: %s" (error-message-string err))
     nil)))

;;; Test 2: Model Fetching

(defun hf-test-fetch-models ()
  "Test fetching models from HuggingFace API."
  (interactive)
  (message "Testing model fetching...")
  (condition-case err
      (let* ((filters '((task . "text-generation") (limit . 5)))
             (models (huggingface-browser--fetch-models-sync filters)))
        (if (and models (listp models) (> (length models) 0))
            (progn
              (message "✓ Model fetch test PASSED: Retrieved %d models" (length models))
              (message "  Sample model: %s" (plist-get (car models) :id))
              t)
          (message "✗ Model fetch test FAILED: No models returned")
          nil))
    (error
     (message "✗ Model fetch test FAILED: %s" (error-message-string err))
     nil)))

;;; Test 3: Model Info Extraction

(defun hf-test-model-info-extraction ()
  "Test extracting model information."
  (interactive)
  (message "Testing model info extraction...")
  (let* ((sample-model '(:id "test/model"
                         :modelId "test/model"
                         :pipeline_tag "text-generation"
                         :tags ("pytorch" "llama")
                         :downloads 12345
                         :likes 678
                         :description "A test model for testing purposes"))
         (info (huggingface-browser--extract-model-info sample-model)))
    (if (and (plist-get info :id)
             (plist-get info :name)
             (plist-get info :type))
        (progn
          (message "✓ Model info extraction test PASSED")
          (message "  ID: %s" (plist-get info :id))
          (message "  Type: %s" (plist-get info :type))
          t)
      (message "✗ Model info extraction test FAILED")
      nil)))

;;; Test 4: Browser Buffer Creation

(defun hf-test-browser-buffer ()
  "Test creating and populating browser buffer."
  (interactive)
  (message "Testing browser buffer creation...")
  (condition-case err
      (let ((buffer (get-buffer-create "*HF-Test-Browser*")))
        (with-current-buffer buffer
          (huggingface-browser-mode)
          (if (derived-mode-p 'tabulated-list-mode)
              (progn
                (message "✓ Browser buffer test PASSED")
                (kill-buffer buffer)
                t)
            (message "✗ Browser buffer test FAILED: Wrong mode")
            (kill-buffer buffer)
            nil)))
    (error
     (message "✗ Browser buffer test FAILED: %s" (error-message-string err))
     nil)))

;;; Test 5: Model Definition Creation

(defun hf-test-model-definition ()
  "Test creating HuggingFace model definitions for chatgpt-shell."
  (interactive)
  (message "Testing model definition creation...")
  (condition-case err
      (if (fboundp 'chatgpt-shell-huggingface--make-model)
          (let ((model (chatgpt-shell-huggingface--make-model
                        :version "test/model"
                        :short-version "test-model"
                        :token-width 4
                        :context-window 2048)))
            (if (and (map-elt model :provider)
                     (map-elt model :version)
                     (map-elt model :handler))
                (progn
                  (message "✓ Model definition test PASSED")
                  (message "  Provider: %s" (map-elt model :provider))
                  (message "  Version: %s" (map-elt model :version))
                  t)
              (message "✗ Model definition test FAILED: Missing fields")
              nil))
        (message "⊘ Model definition test SKIPPED: chatgpt-shell-huggingface not loaded")
        nil)
    (error
     (message "✗ Model definition test FAILED: %s" (error-message-string err))
     nil)))

;;; Test 6: Payload Construction

(defun hf-test-payload-construction ()
  "Test constructing API payload for HuggingFace."
  (interactive)
  (message "Testing payload construction...")
  (condition-case err
      (if (fboundp 'chatgpt-shell-huggingface--make-payload)
          (let* ((model '((:max-new-tokens . 512)))
                 (context '(("Hello" . nil)))
                 (settings '((:temperature . 0.7) (:system-prompt . "Be helpful")))
                 (payload (chatgpt-shell-huggingface--make-payload
                           :model model
                           :context context
                           :settings settings)))
            (if (and (assoc 'inputs payload)
                     (assoc 'parameters payload))
                (progn
                  (message "✓ Payload construction test PASSED")
                  t)
              (message "✗ Payload construction test FAILED: Missing fields")
              nil))
        (message "⊘ Payload construction test SKIPPED: chatgpt-shell-huggingface not loaded")
        nil)
    (error
     (message "✗ Payload construction test FAILED: %s" (error-message-string err))
     nil)))

;;; Test 7: Response Extraction

(defun hf-test-response-extraction ()
  "Test extracting response from HuggingFace API output."
  (interactive)
  (message "Testing response extraction...")
  (condition-case err
      (if (fboundp 'chatgpt-shell-huggingface--extract-response)
          (let* ((test-output '((:pending . "[{\"generated_text\":\"Hello, world!\"}]")
                                (:filtered . "")))
                 (result (chatgpt-shell-huggingface--extract-response test-output))
                 (filtered (map-elt result :filtered)))
            (if (and filtered (string-match-p "Hello" filtered))
                (progn
                  (message "✓ Response extraction test PASSED")
                  (message "  Extracted: %s" filtered)
                  t)
              (message "✗ Response extraction test FAILED: Could not extract response")
              nil))
        (message "⊘ Response extraction test SKIPPED: chatgpt-shell-huggingface not loaded")
        nil)
    (error
     (message "✗ Response extraction test FAILED: %s" (error-message-string err))
     nil)))

;;; Test Suite Runner

(defun hf-test-run-all ()
  "Run all HuggingFace Browser tests."
  (interactive)
  (let ((tests '(hf-test-api-connectivity
                 hf-test-fetch-models
                 hf-test-model-info-extraction
                 hf-test-browser-buffer
                 hf-test-model-definition
                 hf-test-payload-construction
                 hf-test-response-extraction))
        (passed 0)
        (failed 0)
        (skipped 0))

    (message "\n========================================")
    (message "Running HuggingFace Browser Test Suite")
    (message "========================================\n")

    (dolist (test tests)
      (let ((result (funcall test)))
        (cond
         ((eq result t) (setq passed (1+ passed)))
         ((eq result nil) (setq failed (1+ failed)))
         (t (setq skipped (1+ skipped))))))

    (message "\n========================================")
    (message "Test Results:")
    (message "  Passed:  %d" passed)
    (message "  Failed:  %d" failed)
    (message "  Skipped: %d" skipped)
    (message "========================================\n")

    (if (= failed 0)
        (message "✓ All tests passed!")
      (message "✗ Some tests failed. Check output above."))))

;;; Demo Functions

(defun hf-demo-browse-models ()
  "Demo: Browse popular text generation models."
  (interactive)
  (message "Starting demo: Browse popular text generation models")
  (let ((huggingface-browser-default-filters
         '((task . "text-generation")
           (sort . "downloads")
           (limit . 20))))
    (huggingface-browser)))

(defun hf-demo-browse-conversational ()
  "Demo: Browse conversational models."
  (interactive)
  (message "Starting demo: Browse conversational models")
  (let ((huggingface-browser-default-filters
         '((task . "conversational")
           (sort . "likes")
           (limit . 15))))
    (huggingface-browser)))

(defun hf-demo-list-providers ()
  "Demo: List all available chatgpt-shell providers including HuggingFace."
  (interactive)
  (if (boundp 'chatgpt-shell-models)
      (let ((providers (delete-dups
                        (mapcar (lambda (model)
                                  (map-elt model :provider))
                                chatgpt-shell-models))))
        (message "Available providers in chatgpt-shell:")
        (dolist (provider providers)
          (when provider
            (let* ((count (length (seq-filter
                                   (lambda (m)
                                     (string= (map-elt m :provider) provider))
                                   chatgpt-shell-models))))
              (message "  - %s (%d models)" provider count)))))
    (message "chatgpt-shell not loaded")))

(defun hf-demo-show-default-models ()
  "Demo: Show default HuggingFace models."
  (interactive)
  (if (fboundp 'chatgpt-shell-huggingface-models)
      (let ((models (chatgpt-shell-huggingface-models)))
        (message "Default HuggingFace models (%d total):" (length models))
        (dolist (model models)
          (message "  - %s (%s)"
                   (map-elt model :version)
                   (map-elt model :short-version))))
    (message "chatgpt-shell-huggingface not loaded")))

;;; Interactive Help

(defun hf-help ()
  "Show help for HuggingFace Browser tests and demos."
  (interactive)
  (with-current-buffer (get-buffer-create "*HuggingFace Browser Help*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "HuggingFace Browser - Tests & Demos\n" 'face 'bold))
      (insert (make-string 50 ?=) "\n\n")

      (insert (propertize "Test Functions:\n" 'face 'bold))
      (insert "  M-x hf-test-run-all               - Run all tests\n")
      (insert "  M-x hf-test-api-connectivity      - Test API connectivity\n")
      (insert "  M-x hf-test-fetch-models          - Test model fetching\n")
      (insert "  M-x hf-test-model-info-extraction - Test info extraction\n")
      (insert "  M-x hf-test-browser-buffer        - Test browser buffer\n")
      (insert "  M-x hf-test-model-definition      - Test model definitions\n")
      (insert "  M-x hf-test-payload-construction  - Test payload creation\n")
      (insert "  M-x hf-test-response-extraction   - Test response parsing\n\n")

      (insert (propertize "Demo Functions:\n" 'face 'bold))
      (insert "  M-x hf-demo-browse-models         - Browse top text-gen models\n")
      (insert "  M-x hf-demo-browse-conversational - Browse conversational models\n")
      (insert "  M-x hf-demo-list-providers        - List all providers\n")
      (insert "  M-x hf-demo-show-default-models   - Show default HF models\n\n")

      (insert (propertize "Main Commands:\n" 'face 'bold))
      (insert "  M-x huggingface-browser           - Open browser\n")
      (insert "  M-x chatgpt-shell                 - Open chat shell\n")
      (insert "  M-x chatgpt-shell-swap-model      - Swap model\n\n")

      (insert (propertize "Press 'q' to close this window.\n" 'face 'italic))
      (goto-char (point-min)))
    (view-mode)
    (pop-to-buffer (current-buffer))))

(provide 'test-demo)

;;; test-demo.el ends here
