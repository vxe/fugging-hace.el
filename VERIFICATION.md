# Verifying Your HuggingFace Model

This guide shows you **exactly** how to verify that HuggingFace models are being used in your chatgpt-shell.

## Quick Verification Checklist

### ✓ 1. Check the Shell Prompt

When you start a HuggingFace shell, you'll see:

```
[Using HuggingFace model: meta-llama/Meta-Llama-3-8B-Instruct]
[API: https://api-inference.huggingface.co/models/meta-llama/Meta-Llama-3-8B-Instruct]

ChatGPT>
```

**Look for:**
- The model ID (e.g., `meta-llama/Meta-Llama-3-8B-Instruct`)
- The HuggingFace API URL
- These appear in gray/comment color when you launch

### ✓ 2. Check Current Model Info

Run this command at any time:

```
M-x chatgpt-shell-huggingface-current-model
```

**What you'll see:**
```
🤗 HuggingFace Model Information
==================================================

Model ID: meta-llama/Meta-Llama-3-8B-Instruct
Provider: HuggingFace
Short Name: Llama-3-8B
API Endpoint: https://api-inference.huggingface.co/models/meta-llama/Meta-Llama-3-8B-Instruct
Context Window: 8192 tokens
Max Output: 2048 tokens
Token Width: ~4 chars/token

Description:
Meta's Llama 3 8B instruction-tuned model
```

**If you see a different provider:**
```
Current model is NOT a HuggingFace model: gpt-4 (OpenAI)
```

This tells you you're using OpenAI, not HuggingFace.

### ✓ 3. Verify API Connection

Test that your HuggingFace token works:

```
M-x chatgpt-shell-huggingface-verify-api
```

**Success looks like:**
```
✅ HuggingFace API is working!
Authenticated as: your-username (user)
```

**Failure looks like:**
```
❌ API Error: (error http 401)
Check your token in chatgpt-shell-huggingface-key
```

If you see an error:
1. Check your `.authinfo` file has the correct token
2. Verify token at: https://huggingface.co/settings/tokens
3. Test retrieval: `M-: (funcall chatgpt-shell-huggingface-key)`

### ✓ 4. List All HuggingFace Models

See which HuggingFace models are available and which is active:

```
M-x chatgpt-shell-huggingface-list-models
```

**Output:**
```
🤗 Available HuggingFace Models
==================================================

Total: 8 models

▸ Llama-3-8B
  meta-llama/Meta-Llama-3-8B-Instruct
  Meta's Llama 3 8B instruction-tuned model

  Llama-2-7b-chat
  meta-llama/Llama-2-7b-chat-hf
  Meta's Llama 2 7B chat model

  Mistral-7B-v0.2
  mistralai/Mistral-7B-Instruct-v0.2
  Mistral AI's 7B instruction model v0.2

▸ = currently active
```

The `▸` marker shows which model is active.

### ✓ 5. Check chatgpt-shell Variable

Evaluate this in Emacs:

```elisp
M-: chatgpt-shell-model-version
```

**HuggingFace model:**
```
"meta-llama/Meta-Llama-3-8B-Instruct"
```

**Not HuggingFace:**
```
"gpt-4"  ; OpenAI
"claude-sonnet-4-5-20250929"  ; Anthropic
```

HuggingFace model IDs always have a `/` (username/model-name format).

### ✓ 6. Watch the API Requests (Advanced)

Enable logging to see actual API calls:

```elisp
;; In your config or eval:
(setq chatgpt-shell-logging t)
```

Then send a message and check the log:

```
M-x chatgpt-shell-huggingface-show-request-log
```

**Look for:**
```
Request: POST https://api-inference.huggingface.co/models/meta-llama/Meta-Llama-3-8B-Instruct
Headers:
  Authorization: Bearer hf_...
  Content-Type: application/json
Body:
  {"inputs": "User: Your message here\nAssistant:", "parameters": {"max_new_tokens": 512, ...}}
```

You should see:
- URL contains `api-inference.huggingface.co`
- Model name in the URL path
- Your message in the `inputs` field

## Common Issues & Solutions

### Issue 1: Prompt Says "ChatGPT" Not "HuggingFace"

**Symptom:** Shell prompt just says `ChatGPT>` with no model info

**Cause:** You started the shell before selecting a HuggingFace model

**Solution:**
1. Run `M-x chatgpt-shell-huggingface-current-model`
2. If it says "No model currently active", select one:
   - `M-x huggingface-browser` → select model → press `RET`
   - Or `M-x chatgpt-shell-swap-model` → choose HuggingFace model

### Issue 2: Response Looks Wrong

**Symptom:** Responses don't match the model's style/capabilities

**Check which model is ACTUALLY running:**
```
M-x chatgpt-shell-huggingface-current-model
```

**Common causes:**
- You're in an old shell buffer that was started with a different model
- `chatgpt-shell-model-version` wasn't set before starting the shell
- Multiple shells open - you're looking at the wrong one

**Solution:**
1. Close the current shell: `C-x k RET`
2. Start fresh: `M-x huggingface-browser` → select model → `RET`
3. Verify with `M-x chatgpt-shell-huggingface-current-model`

### Issue 3: Error "Model is Loading"

**Symptom:** Response says model is loading, wait 20-60 seconds

**Cause:** HuggingFace Inference API cold starts (first use of a model)

**This is NORMAL!** It proves HuggingFace is being used.

**What happens:**
1. First request: Model loads (20-60s)
2. Subsequent requests: Fast responses (~1-5s)
3. After ~15min idle: Model unloads, next request is slow again

**Verification:** Wait 60s and try again. If it works, HuggingFace is definitely being used!

### Issue 4: Getting OpenAI/Claude Responses Instead

**Symptom:** Response style matches GPT-4 or Claude, not your selected model

**Diagnosis steps:**

1. **Check current model:**
   ```
   M-: chatgpt-shell-model-version
   ```

2. **Check if it's HuggingFace:**
   ```
   M-x chatgpt-shell-huggingface-current-model
   ```

3. **If it says "NOT a HuggingFace model":**
   - The shell is using a different provider
   - Swap models: `M-x chatgpt-shell-swap-model`
   - Choose a model with "HuggingFace" provider

4. **If it says "HuggingFace" but responses are wrong:**
   - Enable logging: `(setq chatgpt-shell-logging t)`
   - Send a test message
   - Check log: `M-x chatgpt-shell-huggingface-show-request-log`
   - Verify URL contains `huggingface.co`

## Test Prompts to Verify Model Behavior

Different models have different behaviors. Try these:

### Test 1: Model Self-Identification

```
What model are you?
```

**Expected responses:**
- **Llama models:** "I am Llama" or similar
- **Mistral models:** "I am Mistral" or similar
- **GPT-4:** "I am GPT-4" ← Wrong! You're using OpenAI, not HuggingFace
- **Claude:** "I am Claude" ← Wrong! You're using Anthropic

### Test 2: Model-Specific Capabilities

Different models have different training cutoffs and capabilities:

```
What is the latest information you have? What year is your training data from?
```

Compare the answer to the model's documentation on HuggingFace.

### Test 3: Model Response Style

Each model has a distinctive style:

- **Llama 3:** Structured, helpful, often uses numbered lists
- **Mistral:** Concise, technical, direct
- **Llama 2:** Longer responses, more conversational
- **GPT-4:** Very polished, comprehensive

### Test 4: Check Cold Start

**Only HuggingFace Inference API has cold starts:**

1. Select a model you haven't used in 15+ minutes
2. Send any message
3. If you get "Model is loading, please retry in 20-60 seconds" → HuggingFace ✓
4. If you get an immediate response → Likely cached or different API

## Visual Indicators Summary

When HuggingFace is working correctly, you'll see:

| Location | What to Look For |
|----------|------------------|
| Shell startup | `[Using HuggingFace model: model-name]` |
| Message area | `🤗 HuggingFace shell started with model: ...` |
| Model info command | `Provider: HuggingFace` in the popup |
| API verification | `✅ HuggingFace API is working!` |
| Model list | `▸` marker next to current model |
| Log buffer | URLs containing `api-inference.huggingface.co` |

## Quick Troubleshooting Commands

Copy-paste these into Emacs:

```elisp
;; 1. Check current model
M-x chatgpt-shell-huggingface-current-model

;; 2. Verify API works
M-x chatgpt-shell-huggingface-verify-api

;; 3. List all HuggingFace models
M-x chatgpt-shell-huggingface-list-models

;; 4. Check variable value
M-: chatgpt-shell-model-version

;; 5. See if token is set
M-: (funcall chatgpt-shell-huggingface-key)

;; 6. Check logging (enable first if needed)
(setq chatgpt-shell-logging t)
M-x chatgpt-shell-huggingface-show-request-log
```

## Still Unsure?

### The Definitive Test

1. **Enable logging:**
   ```elisp
   (setq chatgpt-shell-logging t)
   ```

2. **Start fresh:**
   ```
   M-x huggingface-browser
   ```
   Select a model, press `RET`

3. **Send a test message:**
   ```
   Hello, can you hear me?
   ```

4. **Check the log:**
   ```
   M-x chatgpt-shell-huggingface-show-request-log
   ```

5. **Look for:**
   ```
   URL: https://api-inference.huggingface.co/models/[your-model]
   ```

   If you see this URL, you're 100% using HuggingFace! ✓

   If you see:
   - `api.openai.com` → You're using OpenAI
   - `api.anthropic.com` → You're using Anthropic
   - `localhost` or other → Local model or different service

## Getting Help

If you're still unsure, gather this info:

```elisp
;; Copy the output of these commands:
M-: chatgpt-shell-model-version
M-x chatgpt-shell-huggingface-current-model
M-x chatgpt-shell-huggingface-list-models
```

And check the log buffer:
```
M-x chatgpt-shell-huggingface-show-request-log
```

Share this info when asking for help!

---

**TL;DR: Run `M-x chatgpt-shell-huggingface-current-model` - it tells you everything!** 🎯
