# Integration Guide

This guide shows how to integrate the HuggingFace Browser with the cloned `chatgpt-shell` repository.

## Architecture Overview

```
fugging-hace.el/
├── huggingface-browser.el          # Browser UI (tabulated-list-mode)
├── chatgpt-shell-huggingface.el    # Provider for chatgpt-shell
└── chatgpt-shell/                  # Cloned from xenodium/chatgpt-shell
    ├── chatgpt-shell.el            # Main shell
    ├── chatgpt-shell-openai.el     # OpenAI provider
    ├── chatgpt-shell-anthropic.el  # Anthropic provider
    ├── chatgpt-shell-ollama.el     # Ollama provider (dynamic loading example)
    └── ... (other providers)
```

## Integration Methods

### Method 1: Standalone (No Modification to chatgpt-shell)

This keeps your HuggingFace integration separate from the cloned repo.

**In your `~/.emacs` or `init.el`:**

```elisp
;; Add both directories to load-path
(add-to-list 'load-path "/path/to/fugging-hace.el")
(add-to-list 'load-path "/path/to/fugging-hace.el/chatgpt-shell")

;; Load chatgpt-shell and its dependencies
(require 'shell-maker)
(require 'chatgpt-shell)

;; Load HuggingFace components
(require 'huggingface-browser)
(require 'chatgpt-shell-huggingface)

;; Set API keys
(setq chatgpt-shell-huggingface-key "hf_your_token_here")

;; Optional: Set default model
(setq chatgpt-shell-model-version "meta-llama/Meta-Llama-3-8B-Instruct")
```

**Pros:**
- No changes to cloned repo (easy to update chatgpt-shell)
- Clean separation of concerns
- Can be disabled by removing from load-path

**Cons:**
- Requires manual configuration
- HuggingFace models not available by default

### Method 2: Copy Provider into chatgpt-shell (Integrated)

Copy the HuggingFace provider into the chatgpt-shell directory.

**Steps:**

1. **Copy the provider file:**
   ```bash
   cp chatgpt-shell-huggingface.el chatgpt-shell/
   ```

2. **Edit `chatgpt-shell/chatgpt-shell.el`:**

   Find the requires section (around line 80) and add:
   ```elisp
   (require 'chatgpt-shell-huggingface)
   ```

   Find `chatgpt-shell--make-default-models` (around line 245) and add:
   ```elisp
   (defun chatgpt-shell--make-default-models ()
     "Create a list of default models by combining models from different providers."
     (append (chatgpt-shell-anthropic-models)
             (chatgpt-shell-deepseek-models)
             (chatgpt-shell-google-models)
             (chatgpt-shell-kagi-models)
             (chatgpt-shell-ollama-models)
             (chatgpt-shell-openai-models)
             (chatgpt-shell-openrouter-models)
             (chatgpt-shell-perplexity-models)
             (chatgpt-shell-huggingface-models)))  ;; ADD THIS LINE
   ```

3. **Configure in your `init.el`:**
   ```elisp
   (add-to-list 'load-path "/path/to/fugging-hace.el/chatgpt-shell")
   (require 'chatgpt-shell)
   (setq chatgpt-shell-huggingface-key "hf_your_token_here")
   ```

**Pros:**
- HuggingFace models available by default
- Fully integrated with chatgpt-shell
- Shows up in `M-x chatgpt-shell-swap-model` automatically

**Cons:**
- Modifies cloned repo (conflicts when updating)
- Need to reapply changes when pulling updates

### Method 3: Fork and Modify (Contribution Path)

Create a fork of chatgpt-shell with HuggingFace support.

**Steps:**

1. Fork https://github.com/xenodium/chatgpt-shell
2. Clone your fork
3. Add `chatgpt-shell-huggingface.el` to the repository
4. Modify `chatgpt-shell.el` as in Method 2
5. Commit and push
6. Optional: Submit a pull request to upstream

**Pros:**
- Can contribute back to the community
- Own version with HuggingFace built-in
- Easy to maintain and update

**Cons:**
- Requires GitHub account and git workflow
- Need to keep fork synced with upstream

### Method 4: Browser Only (Minimal)

Use only the browser without full chatgpt-shell integration.

**In your `init.el`:**

```elisp
(add-to-list 'load-path "/path/to/fugging-hace.el")
(require 'huggingface-browser)
(setq huggingface-browser-api-key "hf_your_token_here")
```

**Use with:**
- `M-x huggingface-browser` - Browse models
- Press `d` to view details
- The browser will work, but chat will use a simple fallback buffer

**Pros:**
- Minimal setup
- No dependencies on chatgpt-shell
- Useful for just browsing models

**Cons:**
- No full chat integration
- No streaming responses

## Recommended Setup (Method 1 + Browser)

This is the best balance of features and maintainability:

```elisp
;; ~/.emacs.d/init.el

;; Install chatgpt-shell from MELPA (official stable version)
(use-package chatgpt-shell
  :ensure t
  :custom
  (chatgpt-shell-openai-key
   (lambda () (auth-source-pass-get 'secret "openai-key"))))

;; Add HuggingFace integration
(use-package huggingface-browser
  :load-path "/path/to/fugging-hace.el"
  :after chatgpt-shell
  :custom
  (huggingface-browser-api-key
   (lambda () (auth-source-pass-get 'secret "huggingface-key")))
  :bind
  ("C-c h b" . huggingface-browser)
  :config
  (require 'chatgpt-shell-huggingface)
  (setq chatgpt-shell-huggingface-key huggingface-browser-api-key))
```

## Testing the Integration

1. **Load test file:**
   ```elisp
   (load-file "/path/to/fugging-hace.el/test-demo.el")
   ```

2. **Run all tests:**
   ```
   M-x hf-test-run-all
   ```

3. **Try demos:**
   ```
   M-x hf-demo-browse-models
   M-x hf-demo-list-providers
   ```

## Verifying Integration

### Check 1: Browser Works
```
M-x huggingface-browser
```
Should show a list of models.

### Check 2: Provider Loaded
```elisp
M-: (fboundp 'chatgpt-shell-huggingface-models)
```
Should return `t`.

### Check 3: Models Available
```elisp
M-: (length (chatgpt-shell-huggingface-models))
```
Should return `8` (or however many default models are defined).

### Check 4: In Swap Menu
```
M-x chatgpt-shell-swap-model
```
Should show HuggingFace models in the list.

## Updating chatgpt-shell

### If Using Method 1 (Standalone):
Just update via MELPA:
```
M-x package-update RET chatgpt-shell
```
No conflicts!

### If Using Method 2 (Integrated):
1. Stash your changes:
   ```bash
   cd chatgpt-shell
   git stash
   ```

2. Pull updates:
   ```bash
   git pull origin main
   ```

3. Reapply your changes:
   ```bash
   git stash pop
   ```

4. Resolve conflicts if any

### If Using Method 3 (Fork):
1. Add upstream remote (if not already):
   ```bash
   git remote add upstream https://github.com/xenodium/chatgpt-shell.git
   ```

2. Fetch and merge:
   ```bash
   git fetch upstream
   git merge upstream/main
   ```

3. Resolve conflicts
4. Push to your fork

## Troubleshooting Integration Issues

### "Cannot find chatgpt-shell"
- Ensure chatgpt-shell is in load-path
- Check: `M-: (locate-library "chatgpt-shell")`

### "HuggingFace models not showing in swap menu"
- Verify provider is loaded: `M-: (fboundp 'chatgpt-shell-huggingface-models)`
- Check if models were added: `M-: chatgpt-shell-models`
- If using Method 2, verify you edited `chatgpt-shell--make-default-models`

### "Browser works but chat doesn't"
- Check chatgpt-shell is loaded: `M-: (fboundp 'chatgpt-shell)`
- Verify API key is set: `M-: chatgpt-shell-huggingface-key`
- Check *Messages* buffer for errors

## Contributing

If you want to contribute HuggingFace support to the official chatgpt-shell:

1. Use Method 3 (Fork)
2. Test thoroughly with `test-demo.el`
3. Follow chatgpt-shell's contribution guidelines
4. Submit a well-documented pull request

## Files Summary

| File | Purpose | Required? |
|------|---------|-----------|
| `huggingface-browser.el` | Browser UI | Yes (for browser) |
| `chatgpt-shell-huggingface.el` | Provider | Yes (for chat) |
| `test-demo.el` | Tests/Demos | No (dev only) |
| `example-config.el` | Config examples | No (reference) |
| `README.md` | Documentation | No (reference) |
| `QUICKSTART.md` | Quick guide | No (reference) |

## Next Steps

1. Choose your integration method
2. Configure according to the method
3. Test with `hf-test-run-all`
4. Try browsing with `M-x huggingface-browser`
5. Launch a chat session
6. Enjoy! 🎉

For questions or issues, refer to:
- [README.md](README.md) - Full documentation
- [QUICKSTART.md](QUICKSTART.md) - Quick setup
- [example-config.el](example-config.el) - Configuration examples
