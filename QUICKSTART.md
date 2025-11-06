# Quick Start Guide

Get up and running with HuggingFace Browser in 5 minutes!

## Step 1: Get a HuggingFace Token (Free)

1. Go to https://huggingface.co/join
2. Create a free account (or sign in)
3. Navigate to https://huggingface.co/settings/tokens
4. Click "New token"
5. Name it "emacs-browser" and select "Read" permissions
6. Copy the token (starts with `hf_`)

## Step 2: Install chatgpt-shell (Optional but Recommended)

In Emacs:

```
M-x package-install RET chatgpt-shell RET
```

Or add to your `init.el`:

```elisp
(use-package chatgpt-shell
  :ensure t)
```

## Step 3: Add to Your Emacs Config

Add these lines to `~/.emacs` or `~/.emacs.d/init.el`:

```elisp
;; Add to load path
(add-to-list 'load-path "/path/to/fugging-hace.el")

;; Load the browser
(require 'huggingface-browser)

;; Load chatgpt-shell integration (if installed)
(when (require 'chatgpt-shell nil t)
  (require 'chatgpt-shell-huggingface)
  ;; Set your token (replace with your actual token)
  (setq chatgpt-shell-huggingface-key "hf_xxxxxxxxxxxxxxxxxxxxx"))

;; Optional: keybinding for quick access
(global-set-key (kbd "C-c h b") #'huggingface-browser)
```

**Important**: Replace `"hf_xxxxxxxxxxxxxxxxxxxxx"` with your actual token!

## Step 4: Restart Emacs

Close and reopen Emacs, or evaluate the config:

```
M-x eval-buffer RET
```

## Step 5: Launch the Browser!

```
M-x huggingface-browser RET
```

Or use the keybinding: `C-c h b`

## Step 6: Browse and Chat

1. **Navigate**: Use arrow keys or `n`/`p` to browse models
2. **View Details**: Press `d` on any model to see full information
3. **Launch Chat**: Press `RET` (Enter) on any model to start chatting
4. **Filter**: Press `f` to change filters (task type, sort order, etc.)
5. **Refresh**: Press `r` to fetch latest models from HuggingFace

## Quick Examples

### Example 1: Chat with Llama 3

1. `M-x huggingface-browser`
2. Find "Meta-Llama-3-8B-Instruct" in the list
3. Press `RET`
4. Type: `What is machine learning?`
5. Press `RET` to send
6. Watch the response stream in!

### Example 2: Browse by Task

1. `M-x huggingface-browser`
2. Press `f` (set filter)
3. Task: `conversational`
4. Sort by: `likes`
5. Limit: `25`
6. Browse the top 25 conversational models!

### Example 3: View Model Details

1. `M-x huggingface-browser`
2. Navigate to any model
3. Press `d`
4. See full description, download stats, tags, etc.
5. Press `q` to close

## Troubleshooting

### "Failed to fetch models"

- Check your internet connection
- Try: `curl "https://huggingface.co/api/models?limit=1"`
- If curl works, it's an Emacs configuration issue

### "HuggingFace API key not set"

- Make sure you added `(setq chatgpt-shell-huggingface-key "hf_...")` to your config
- Restart Emacs or run `M-x eval-buffer`

### No models showing up

- Press `f` in the browser
- Leave "Task" empty (press RET without typing)
- Set limit to 100
- This will show all model types

### Chat not working

- Ensure chatgpt-shell is installed: `M-x package-list-packages` and search for "chatgpt-shell"
- Check that chatgpt-shell-huggingface.el is loaded: `M-x locate-library RET chatgpt-shell-huggingface RET`

## Next Steps

- Read the full [README.md](README.md) for advanced features
- Check [example-config.el](example-config.el) for configuration examples
- Explore different model types (summarization, translation, etc.)
- Try multiple models and compare responses!

## Keybinding Quick Reference

**In Browser:**
- `RET` - Launch chat with model
- `d` - View details
- `f` - Set filters
- `r` / `g` - Refresh
- `q` - Quit
- `n` / `p` - Next/Previous

**Global (if configured):**
- `C-c h b` - Open browser
- `C-c h s` - Open chatgpt-shell

## Security Tip

Don't hardcode your API key! Use one of these secure methods:

```elisp
;; Method 1: Environment variable
(setq chatgpt-shell-huggingface-key (getenv "HUGGINGFACE_API_KEY"))

;; Method 2: Auth-source (in ~/.authinfo.gpg)
(setq chatgpt-shell-huggingface-key
      (lambda ()
        (auth-source-pick-first-password
         :host "api-inference.huggingface.co")))
```

See [example-config.el](example-config.el) for more secure storage options.

---

**That's it! Happy browsing! 🤗**

For questions or issues, check the [README.md](README.md) or open an issue on GitHub.
