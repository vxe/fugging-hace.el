# Secure Token Storage with .authinfo

This guide shows how to securely store your HuggingFace API token using Emacs' built-in `auth-source` library with `.authinfo` or `.authinfo.gpg`.

## Why Use .authinfo?

**Benefits:**
- ✓ No hardcoded secrets in your config files
- ✓ Encrypted storage with `.authinfo.gpg`
- ✓ One central location for all API keys
- ✓ Works across multiple Emacs packages
- ✓ Built-in Emacs support (no extra packages)
- ✓ Can be version-controlled safely (when encrypted)

## Quick Setup (3 Steps)

### Step 1: Create .authinfo File

Choose either **unencrypted** (simpler) or **encrypted** (recommended):

#### Option A: Unencrypted (Quick Start)

```bash
# Create the file
touch ~/.authinfo

# Set restrictive permissions (important!)
chmod 600 ~/.authinfo

# Add your token
echo "machine api-inference.huggingface.co login token password hf_YourActualTokenHere" >> ~/.authinfo
```

#### Option B: Encrypted (Recommended)

```bash
# Create encrypted file
touch ~/.authinfo.gpg

# Emacs will prompt for encryption password when you save
```

Then edit in Emacs:
```elisp
C-x C-f ~/.authinfo.gpg RET
```

Add this line:
```
machine api-inference.huggingface.co login token password hf_YourActualTokenHere
```

Save (`C-x C-s`) and Emacs will encrypt it automatically.

### Step 2: Configure Emacs to Use .authinfo

Add to your `~/.emacs` or `~/.emacs.d/init.el`:

```elisp
;; Load HuggingFace browser
(require 'huggingface-browser)
(require 'chatgpt-shell-huggingface)

;; Configure to read from .authinfo
(setq chatgpt-shell-huggingface-key
      (lambda ()
        (auth-source-pick-first-password
         :host "api-inference.huggingface.co")))

;; Optional: Set the same for browser API key
(setq huggingface-browser-api-key chatgpt-shell-huggingface-key)
```

### Step 3: Test It

```elisp
;; Test that the key is retrieved correctly
M-: (funcall chatgpt-shell-huggingface-key)
```

Should return your token (starts with `hf_`).

## .authinfo File Format

### Basic Format

```
machine HOST login USERNAME password SECRET
```

### HuggingFace Examples

**Standard format:**
```
machine api-inference.huggingface.co login token password hf_YourTokenHere
```

**Alternative (using main hub):**
```
machine huggingface.co login myusername password hf_YourTokenHere
```

**With port specification:**
```
machine api-inference.huggingface.co port 443 login token password hf_YourTokenHere
```

### Multiple Services in One File

You can store multiple API keys:

```
# HuggingFace
machine api-inference.huggingface.co login token password hf_YourHFToken

# OpenAI
machine api.openai.com login apikey password sk-YourOpenAIKey

# Anthropic
machine api.anthropic.com login apikey password sk-ant-YourAnthropicKey

# GitHub
machine api.github.com login your-username password ghp_YourGitHubToken
```

## Configuration Methods

### Method 1: Basic (Function)

```elisp
(setq chatgpt-shell-huggingface-key
      (lambda ()
        (auth-source-pick-first-password
         :host "api-inference.huggingface.co")))
```

**Pros:** Simple, lazy-loaded (no prompt until first use)
**Cons:** Prompts for password each Emacs session (if using .gpg)

### Method 2: With Login Matching

```elisp
(setq chatgpt-shell-huggingface-key
      (lambda ()
        (auth-source-pick-first-password
         :host "api-inference.huggingface.co"
         :user "token")))
```

**Pros:** More specific matching
**Cons:** Must match login field in .authinfo exactly

### Method 3: Cache the Password

```elisp
(setq chatgpt-shell-huggingface-key
      (lambda ()
        (or chatgpt-shell-huggingface--cached-key
            (setq chatgpt-shell-huggingface--cached-key
                  (auth-source-pick-first-password
                   :host "api-inference.huggingface.co")))))
```

**Pros:** Only prompts once per session
**Cons:** Cached in memory (less secure if Emacs is compromised)

### Method 4: Using auth-source-search (Advanced)

```elisp
(setq chatgpt-shell-huggingface-key
      (lambda ()
        (when-let* ((auth-info (car (auth-source-search
                                     :host "api-inference.huggingface.co"
                                     :max 1
                                     :require '(:secret))))
                    (secret (plist-get auth-info :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))))
```

**Pros:** Full control over auth-source behavior
**Cons:** More verbose

## Using .authinfo.gpg (Encrypted)

### Setup GPG Encryption

#### First Time Setup

1. **Install GPG** (if not already installed):
   ```bash
   # macOS
   brew install gnupg

   # Ubuntu/Debian
   sudo apt-get install gnupg

   # Fedora
   sudo dnf install gnupg
   ```

2. **Generate a key** (if you don't have one):
   ```bash
   gpg --full-generate-key
   # Choose defaults, add your name and email
   ```

3. **Configure Emacs** to use GPG:
   ```elisp
   ;; In your init.el
   (require 'epa-file)
   (epa-file-enable)

   ;; Optional: Choose GPG program
   (setq epg-gpg-program "gpg2")  ; or "gpg"
   ```

#### Create Encrypted .authinfo.gpg

```elisp
;; In Emacs:
C-x C-f ~/.authinfo.gpg RET

;; Add your credentials:
machine api-inference.huggingface.co login token password hf_YourTokenHere

;; Save (C-x C-s)
;; Emacs will prompt:
;;   "Select recipients for encryption"
;; Choose your GPG key from the list

;; Enter your GPG passphrase
```

#### Decryption Behavior

**First access per session:**
- Emacs prompts for your GPG passphrase
- Password is cached for the session (configurable)

**Configure caching:**
```elisp
;; Cache passphrase for 10 minutes
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(setq password-cache-expiry 600)  ; 600 seconds = 10 minutes

;; Or disable caching (more secure)
(setq epa-file-cache-passphrase-for-symmetric-encryption nil)
```

## Troubleshooting

### "No secret found" Error

**Problem:** `auth-source-pick-first-password` returns nil

**Solutions:**

1. **Check file exists:**
   ```elisp
   M-: (file-exists-p "~/.authinfo")
   M-: (file-exists-p "~/.authinfo.gpg")
   ```

2. **Verify auth-sources list:**
   ```elisp
   M-: auth-sources
   ;; Should include "~/.authinfo" or "~/.authinfo.gpg"
   ```

3. **Check file format:**
   - Ensure no extra spaces
   - Use tabs or single spaces between fields
   - No trailing whitespace

4. **Test retrieval manually:**
   ```elisp
   M-: (auth-source-pick-first-password :host "api-inference.huggingface.co")
   ```

### GPG Prompts Not Showing

**Problem:** Emacs freezes when accessing .authinfo.gpg

**Solution 1: Use pinentry in terminal**
```elisp
(setenv "GPG_AGENT_INFO" nil)
```

**Solution 2: Install pinentry-emacs**
```bash
# macOS
brew install pinentry-mac

# Linux
sudo apt-get install pinentry-tty
```

Configure GPG to use it:
```bash
echo "pinentry-program $(which pinentry-mac)" >> ~/.gnupg/gpg-agent.conf
# Or for Linux: pinentry-tty
gpgconf --kill gpg-agent  # Restart agent
```

### Wrong Password Cached

**Problem:** Changed password but Emacs uses old one

**Solution:**
```elisp
;; Clear auth-source cache
M-: (auth-source-forget-all-cached)

;; Or clear password cache
M-: (password-cache-remove "~/.authinfo.gpg")
```

### File Permissions Warning

**Problem:** Warning about .authinfo permissions

**Solution:**
```bash
chmod 600 ~/.authinfo
# Or for .gpg:
chmod 600 ~/.authinfo.gpg
```

## Security Best Practices

### 1. Use Encryption
✓ **Do:** Use `.authinfo.gpg` instead of `.authinfo`
✗ **Don't:** Store tokens in plain text if possible

### 2. Set Correct Permissions
```bash
chmod 600 ~/.authinfo
chmod 600 ~/.authinfo.gpg
```

### 3. Never Commit to Git
Add to `.gitignore`:
```gitignore
.authinfo
.authinfo.gpg
```

### 4. Use Different Tokens per Device
Create separate HuggingFace tokens for:
- Work laptop
- Personal laptop
- Server/remote machines

Revoke compromised tokens at: https://huggingface.co/settings/tokens

### 5. Limit Token Scope
When creating tokens:
- Choose "Read" if you only need model access
- Don't use "Write" unless necessary
- Set expiration dates for temporary use

### 6. Regular Rotation
Rotate tokens periodically:
```bash
# Every 90 days:
# 1. Create new token on HuggingFace
# 2. Update .authinfo
# 3. Test new token works
# 4. Revoke old token
```

## Migration from Hardcoded Tokens

If you currently have tokens hardcoded in your config:

### Step 1: Create .authinfo
```bash
# Extract your current token value
# Add to .authinfo.gpg (encrypted)
```

### Step 2: Update Config
**Before:**
```elisp
(setq chatgpt-shell-huggingface-key "hf_hardcoded_token_here")
```

**After:**
```elisp
(setq chatgpt-shell-huggingface-key
      (lambda ()
        (auth-source-pick-first-password
         :host "api-inference.huggingface.co")))
```

### Step 3: Remove Hardcoded Token
```bash
# Search for any remaining hardcoded tokens
grep -r "hf_" ~/.emacs.d/
grep -r "hf_" ~/.emacs

# Remove them!
```

### Step 4: Test
```elisp
M-x huggingface-browser
# Should work without errors
```

## Complete Example Config

Here's a complete, secure configuration:

```elisp
;; ~/.emacs.d/init.el

;; Enable GPG encryption support
(require 'epa-file)
(epa-file-enable)

;; Configure auth-source
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))

;; Load HuggingFace browser
(use-package huggingface-browser
  :load-path "~/path/to/fugging-hace.el"
  :after chatgpt-shell
  :custom
  ;; Read API key from .authinfo.gpg
  (chatgpt-shell-huggingface-key
   (lambda ()
     (auth-source-pick-first-password
      :host "api-inference.huggingface.co")))

  ;; Use same key for browser
  (huggingface-browser-api-key chatgpt-shell-huggingface-key)

  :bind
  ("C-c h b" . huggingface-browser)

  :config
  (require 'chatgpt-shell-huggingface))
```

And your `~/.authinfo.gpg`:
```
machine api-inference.huggingface.co login token password hf_YourActualTokenHere
```

## Alternative: Using pass (password-store)

If you prefer `pass`, see the `example-config.el` file for integration examples.

## Getting Your HuggingFace Token

1. Go to https://huggingface.co/settings/tokens
2. Click "New token"
3. Name it (e.g., "emacs-browser")
4. Choose "Read" permission (sufficient for browsing and inference)
5. Click "Generate"
6. Copy the token (starts with `hf_`)
7. Add to your `.authinfo.gpg`

## Resources

- Emacs auth-source docs: `C-h i m auth RET`
- GPG documentation: https://gnupg.org/documentation/
- HuggingFace tokens: https://huggingface.co/docs/hub/security-tokens

---

**You're now using secure token storage! 🔒**
