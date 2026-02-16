# ob-prompt.el

**Org Babel meets LLMs.** Write prompts as source blocks, execute with `C-c C-c`, get results inline.

```org
#+begin_src prompt :model claude-sonnet-4-5-20250929
Explain monads in simple terms.
#+end_src
```

> **[Read the full documentation](https://pamuz.github.io/ob-prompt/htmldocs/)**

## Features

- Any **OpenAI-compatible endpoint** — OpenAI, Azure, Anthropic via proxy, local models
- **Preamble context** — automatically feeds buffer content above the block as context
- **Noweb references** — compose prompts from other named blocks with `<<block-name>>`
- **Variable expansion** — inject `:var` bindings into your prompt with `$name`
- **Debug mode** — inspect the exact request payload without making an API call
- **Literate source** — the entire package is tangled from a single Org file

## Requirements

- Emacs 28.1+
- Org 9.7+

## Installation

Clone the repo and add it to your load path:

```emacs-lisp
(add-to-list 'load-path "/path/to/ob-prompt")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((prompt . t)))
```

## Quick start

Set your defaults once per file:

```org
#+PROPERTY: header-args:prompt :model gpt-4 :endpoint "https://api.openai.com/v1/chat/completions" :api-key (getenv "OPENAI_API_KEY")
```

Then write prompts:

```org
#+begin_src prompt
What is the capital of France?
#+end_src
```

### Header arguments

| Argument    | Description                                              |
|-------------|----------------------------------------------------------|
| `:model`    | Model identifier (e.g. `claude-sonnet-4-5-20250929`, `gpt-4`)         |
| `:endpoint` | API endpoint URL                                         |
| `:api-key`  | Key as a string or an evaluated form                     |
| `:system`   | System prompt                                            |
| `:preamble` | When `yes`, include buffer content above block as context|
| `:debug`    | Show the request payload without calling the API         |

## Running tests

```bash
emacs -batch -L . -l ob-prompt-test.el -f ert-run-tests-batch-and-exit
```

## Author

Pablo Munoz

- https://x.com/digitalbandido
- https://github.com/pamuz
- contact@slashpablo.com
