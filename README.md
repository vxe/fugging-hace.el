# HuggingFace Browser for Emacs

An interactive browser for HuggingFace models with seamless integration into `chatgpt-shell`. Browse thousands of models, view details, and launch chat sessions directly from Emacs.

## Features

### 🔍 Interactive Model Browser
- **Tabulated List UI**: Scrollable, sortable list of HuggingFace models
- **Rich Information**: View model names, types, download counts, likes, and descriptions
- **Smart Filtering**: Filter by task type (text-generation, conversational, etc.)
- **Caching**: Smart caching with configurable TTL to reduce API calls
- **Keyboard-Driven**: Full keyboard navigation with Emacs-native keybindings

### 💬 Integrated Chat Sessions
- **One-Click Launch**: Press `RET` on any model to start chatting
- **chatgpt-shell Integration**: Leverages the powerful `chatgpt-shell` infrastructure
- **Streaming Responses**: Real-time response streaming from HuggingFace models
- **History & Context**: Maintains conversation context across interactions
- **Multiple Models**: Easily switch between different HuggingFace models

### 🚀 HuggingFace Provider
- **Native Integration**: First-class HuggingFace support in chatgpt-shell
- **Popular Models**: Pre-configured with popular models (Llama, Mistral, Falcon, etc.)
- **Dynamic Addition**: Add any HuggingFace model on the fly
- **Inference API**: Uses HuggingFace's Inference API for responses

## Installation

### Prerequisites

1. **Emacs 27.1+** (required)
2. **chatgpt-shell** (optional but recommended for full functionality)
3. **HuggingFace API Token** (free from https://huggingface.co/settings/tokens)

### Quick Setup

1. **Clone or download** this repository:
   ```bash
   git clone <repository-url>
   cd fugging-hace.el
   ```

2. **Add to your load-path** in `~/.emacs` or `~/.emacs.d/init.el`:
   ```elisp
   (add-to-list 'load-path "/path/to/fugging-hace.el")
   ```

3. **Load the browser**:
   ```elisp
   (require 'huggingface-browser)
   ```

4. **Optional: Load chatgpt-shell integration**:
   ```elisp
   ;; If you have chatgpt-shell installed
   (require 'chatgpt-shell)
   (require 'chatgpt-shell-huggingface)

   ;; Set your HuggingFace API token
   (setq chatgpt-shell-huggingface-key "hf_your_token_here")
   ```

### With use-package

```elisp
(use-package huggingface-browser
  :load-path "/path/to/fugging-hace.el"
  :custom
  (huggingface-browser-api-key "hf_your_token_here")
  :config
  (when (require 'chatgpt-shell nil t)
    (require 'chatgpt-shell-huggingface)
    (setq chatgpt-shell-huggingface-key huggingface-browser-api-key)))
```

## Usage

### Launching the Browser

```
M-x huggingface-browser RET
```

This opens the HuggingFace model browser showing popular text-generation models.

### Browser Keybindings

| Key | Action |
|-----|--------|
| `RET` | Launch chat session with selected model |
| `d` | Show detailed information about model |
| `r` / `g` | Refresh model list from API |
| `f` | Set filters (task, sort order, limit) |
| `q` | Quit browser |
| `n` / `p` | Navigate to next/previous model |
| Arrow keys | Navigate list |
| Mouse click | Select and launch model |

### Filtering Models

Press `f` in the browser to set custom filters:

- **Task**: Filter by specific tasks:
  - `text-generation` - General text generation models
  - `conversational` - Chat/conversation models
  - `text2text-generation` - Sequence-to-sequence models
  - `summarization` - Summarization models
  - `translation` - Translation models
  - And more...

- **Sort By**: Order models by:
  - `downloads` (default) - Most downloaded first
  - `likes` - Most liked first
  - `trending` - Currently trending
  - `createdAt` - Newest first
  - `lastModified` - Recently updated

- **Limit**: Maximum number of models to fetch (default: 100)

### Using chatgpt-shell Integration

Once chatgpt-shell is installed and configured:

1. Browse models: `M-x huggingface-browser`
2. Select a model with `RET`
3. A chatgpt-shell session opens with that model
4. Chat naturally - context is maintained automatically

Alternatively, swap models within chatgpt-shell:

```
M-x chatgpt-shell-swap-model RET
```

All HuggingFace models will appear in the list alongside OpenAI, Anthropic, etc.

### Adding Custom Models

Add any HuggingFace model dynamically:

```elisp
M-x chatgpt-shell-huggingface-add-model RET
Model ID: username/model-name RET
```

Or programmatically:

```elisp
(chatgpt-shell-huggingface-add-model "mistralai/Mistral-7B-Instruct-v0.2")
```

## Configuration

### API Token

**Required for chat functionality**. Get your free token from https://huggingface.co/settings/tokens

```elisp
;; Direct string
(setq chatgpt-shell-huggingface-key "hf_xxxxxxxxxxxxx")

;; Or use a function (recommended for security)
(setq chatgpt-shell-huggingface-key
      (lambda ()
        (auth-source-pass-get 'secret "huggingface-key")))
```

### Default Filters

Customize what models appear by default:

```elisp
(setq huggingface-browser-default-filters
      '((task . "conversational")  ; Only conversational models
        (sort . "likes")            ; Sort by likes
        (direction . -1)            ; Descending order
        (limit . 50)))              ; Fetch 50 models
```

### Cache Settings

Adjust cache behavior:

```elisp
(setq huggingface-browser-cache-ttl 7200)  ; 2 hours in seconds
```

### API URLs

Override default API endpoints if needed:

```elisp
(setq huggingface-browser-api-url "https://huggingface.co/api/models")
(setq huggingface-browser-inference-url "https://api-inference.huggingface.co/models")
(setq chatgpt-shell-huggingface-api-url-base "https://api-inference.huggingface.co")
```

## Examples

### Example 1: Quick Chat Session

```
M-x huggingface-browser RET
;; Navigate to "Meta-Llama-3-8B-Instruct"
RET
;; Type your prompt
What is the capital of France?
RET
;; Model responds: "The capital of France is Paris."
```

### Example 2: Browse by Task

```
M-x huggingface-browser RET
f  ;; Set filters
Task: summarization RET
Sort by: downloads RET
Limit: 25 RET
;; Now showing top 25 summarization models
```

### Example 3: Model Details

```
M-x huggingface-browser RET
;; Navigate to any model
d  ;; View details
;; See full description, tags, statistics
```

### Example 4: Use with chatgpt-shell

```elisp
;; In your init.el
(use-package chatgpt-shell
  :ensure t
  :config
  (require 'chatgpt-shell-huggingface)
  (setq chatgpt-shell-huggingface-key "hf_xxxxx")
  (setq chatgpt-shell-model-version "meta-llama/Meta-Llama-3-8B-Instruct"))

;; Now M-x chatgpt-shell starts with Llama 3
;; Or M-x chatgpt-shell-swap-model to choose any model
```

## Architecture

### Components

1. **huggingface-browser.el**: Main browser UI using `tabulated-list-mode`
   - Model discovery via HuggingFace Hub API
   - Interactive browser with filtering and sorting
   - Model detail views
   - Integration hooks for chat sessions

2. **chatgpt-shell-huggingface.el**: Provider for `chatgpt-shell`
   - HuggingFace Inference API integration
   - Model definitions for popular models
   - Request/response handling
   - Streaming support via shell-maker

### Integration Points

```
┌─────────────────────┐
│  huggingface-       │
│  browser.el         │
│  (UI Layer)         │
└──────────┬──────────┘
           │
           │ Launch model
           │
           ▼
┌─────────────────────┐
│  chatgpt-shell-     │
│  huggingface.el     │
│  (Provider Layer)   │
└──────────┬──────────┘
           │
           │ API calls
           │
           ▼
┌─────────────────────┐
│  HuggingFace        │
│  Inference API      │
└─────────────────────┘
```

### API Usage

The browser queries two HuggingFace endpoints:

1. **Hub API** (`/api/models`): Fetch available models with metadata
2. **Inference API** (`/models/{model-id}`): Send prompts and get responses

## Troubleshooting

### "Failed to fetch models"

- Check your internet connection
- Verify API URL is accessible: https://huggingface.co/api/models
- Try with `curl`: `curl "https://huggingface.co/api/models?task=text-generation&limit=10"`

### "HuggingFace API key not set"

- Set `chatgpt-shell-huggingface-key` in your config
- Get a free token from https://huggingface.co/settings/tokens

### "Please upgrade shell-maker"

- Ensure you have `shell-maker` 0.79.1 or newer
- Update chatgpt-shell: `M-x package-upgrade chatgpt-shell`

### Model not responding

- Some models may be loading (cold start) - wait 30-60 seconds
- Try a different model - some may not support the Inference API
- Check model card on HuggingFace for API availability

### Browser shows no models

- Check `huggingface-browser-default-filters` - it may be too restrictive
- Press `f` and remove task filter (leave empty)
- Try forcing refresh: `C-u M-x huggingface-browser`

## Pre-configured Models

The provider includes these popular models out of the box:

| Model | ID | Description |
|-------|-----|-------------|
| Llama 3 8B | `meta-llama/Meta-Llama-3-8B-Instruct` | Meta's latest instruction model |
| Llama 2 7B | `meta-llama/Llama-2-7b-chat-hf` | Meta's Llama 2 chat model |
| Mistral 7B v0.2 | `mistralai/Mistral-7B-Instruct-v0.2` | Mistral AI's instruction model |
| Mixtral 8x7B | `mistralai/Mixtral-8x7B-Instruct-v0.1` | Mistral's Mixture of Experts |
| FLAN-T5 XXL | `google/flan-t5-xxl` | Google's FLAN-T5 model |
| DialoGPT | `microsoft/DialoGPT-large` | Microsoft's conversational model |
| Falcon 7B | `tiiuae/falcon-7b-instruct` | TII's Falcon instruction model |
| MPT 7B Chat | `mosaicml/mpt-7b-chat` | MosaicML's chat model |

## License

MIT License - see LICENSE file for details

## Contributing

Contributions welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Submit a pull request

## Acknowledgments

- **chatgpt-shell** by @xenodium for the excellent shell infrastructure
- **HuggingFace** for the free Inference API and model hosting
- The Emacs community for `tabulated-list-mode` and other primitives

## Support

- Report issues on GitHub
- Join discussions on the project page
- Check HuggingFace docs: https://huggingface.co/docs/api-inference

---

**Happy model browsing!** 🤗🦙🚀
