;;; ob-prompt.el --- Org Babel functions for LLM prompts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pablo Munoz

;; Author: Pablo Munoz <contact@slashpablo.com>
;; Mantainer: Pablo Munoz <contact@slashpablo.com>
;; URL: https://github.com/slashpablo/ob-prompt
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org "9.7"))
;; Keywords: org, babel, ai, llm

;; This file is part of ob-prompt.
;;
;; ob-prompt is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; ob-prompt is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with ob-prompt.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org Babel support for executing LLM prompts as src blocks.
;;
;; Usage:
;;   #+begin_src prompt :model claude-sonnet-4-5-20250929 :results raw
;;   Explain monads in simple terms.
;;   #+end_src
;;
;; Press C-c C-c on the block to send the prompt to an LLM
;; and insert the response as a #+RESULTS: block.

;;; Code:

(require 'ob)
(require 'org-element)
(require 'org-macs)
(require 'json)
(require 'url)

(defconst org-babel-header-args:prompt
  '((model    . :any)   ;; LLM model identifier
    (system   . :any)   ;; system prompt string
    (endpoint . :any)   ;; API endpoint URL
    (api-key  . :any)   ;; API key (string or evaluated form)
    (preamble . :any)   ;; include buffer content above block as context
    (debug    . :any))  ;; return raw request/response instead of content
  "Prompt-specific header arguments for ob-prompt.")

(defvar org-babel-default-header-args:prompt
  '((:results . "raw")
    (:exports . "both") ;; Make sures both prompt and result are exported
    (:eval . "no-export") ;; Do not re-evaluate prompt blocks on export
    (:wrap . "assistant") ;; Provide concrete boundries to responses
    (:noweb . "yes")) ;; Enable <<block-name>> inclusion by default
  "Default header arguments for prompt src blocks.")

(defun ob-prompt--build-messages (body &optional system preamble-text)
  "Build the messages vector for the API request.
BODY is the user's prompt text.
SYSTEM, when non-nil, becomes a system message.
PREAMBLE-TEXT, when non-nil, is wrapped in <context> tags
and prepended to the user message."
  (let ((user-content (if preamble-text
                          (format "<context>\n%s\n</context>\n\n%s"
                                  preamble-text body)
                        body))
        (messages '()))
    (when system
      (push `((role . "system") (content . ,system)) messages))
    (push `((role . "user") (content . ,user-content)) messages)
    (vconcat (nreverse messages))))

(defun ob-prompt--build-payload (model messages)
  "Build the JSON-ready alist for the API request.
MODEL is the model identifier string.
MESSAGES is the vector returned by `ob-prompt--build-messages'."
  `(("model" . ,model)
    ("max_completion_tokens" . 16384)
    ("messages" . ,messages)))

(defun ob-prompt--parse-response (json-string)
  "Extract the assistant's message content from JSON-STRING.
Signals `user-error' if the response indicates an API error
or has an unexpected structure."
  (let* ((data (condition-case err
                   (json-read-from-string json-string)
                 (json-error
                  (user-error "ob-prompt: failed to parse response JSON: %s"
                              (error-message-string err)))))
         (err-msg (alist-get 'message (alist-get 'error data))))
    (when err-msg
      (user-error "ob-prompt: API error: %s" err-msg))
    (condition-case nil
        (alist-get 'content
                   (alist-get 'message
                              (aref (alist-get 'choices data) 0)))
      (error
       (user-error "ob-prompt: unexpected response structure: %s"
                   (truncate-string-to-width json-string 200))))))

(defun ob-prompt--json-encode-ascii (object)
  "Encode OBJECT as JSON with all non-ASCII characters escaped to \\uNNNN.
This avoids `url-http-create-request' failing with
\"Multibyte text in HTTP request\" (Emacs Bug#23750)."
  (let ((json (json-encode object)))
    (replace-regexp-in-string
     "[^\x00-\x7f]"
     (lambda (char)
       (format "\\u%04x" (string-to-char char)))
     json nil t)))

(defun ob-prompt--request (endpoint api-key payload)
  "Send PAYLOAD to ENDPOINT using API-KEY via built-in `url'.
Returns the response body as a string.
Signals `user-error' on HTTP or transport errors."
  (let* ((url-request-method "POST")
	 (url-request-extra-headers
	  `(("Content-Type" . "application/json")
	    ("Authorization" . ,(concat "Bearer " api-key))))
	 (url-request-data (ob-prompt--json-encode-ascii payload))
	 (buffer (url-retrieve-synchronously endpoint t t 300)))
    (unless buffer
      (user-error "ob-prompt: request failed (no response buffer)"))
    (with-current-buffer buffer
      (unwind-protect
       (progn
	 (goto-char (point-min))
	 ;; Check HTTP status
	 (unless (looking-at "HTTP/.* 200")
	   (re-search-forward "^HTTP/.* \\([0-9]+\\)" nil t)
	   (user-error "ob-prompt: HTTP error %s"
		       (match-string 1)))
	 ;; Skip headers
	 (re-search-forward "\r?\n\r?\n" nil 'move)
	 (decode-coding-string
	  (buffer-substring-no-properties (point) (point-max))
	  'utf-8))
       (kill-buffer buffer)))))

(defun org-babel-execute:prompt (body params)
  "Execute a prompt BODY with PARAMS via Org Babel.
Sends the prompt to the configured LLM endpoint and returns
the response text."
  (let* ((model    (cdr (assq :model params)))
         (endpoint (cdr (assq :endpoint params)))
         (api-key  (cdr (assq :api-key params)))
         (system   (cdr (assq :system params)))
         (preamble (cdr (assq :preamble params)))
         (debug    (cdr (assq :debug params)))
         ;; Gather buffer text above this src block when :preamble is set
         (preamble-text
          (when (equal preamble "yes")
            (let ((src-block (org-element-at-point)))
              (buffer-substring-no-properties
               (point-min)
               (org-element-property :begin src-block)))))
         ;; Expand :var references in the body
         (expanded-body (org-babel-expand-body:prompt body params))
         ;; Assemble the request
         (messages (ob-prompt--build-messages
                    expanded-body system preamble-text))
         (payload  (ob-prompt--build-payload model messages)))
    (if (equal debug "yes")
        (ob-prompt--format-debug endpoint api-key payload)
      (ob-prompt--parse-response
       (ob-prompt--request endpoint api-key payload)))))

(defun ob-prompt--format-debug (endpoint api-key payload)
  "Format a debug representation of the request.
Redacts API-KEY to show only the first and last 4 characters."
  (let ((redacted (if (and api-key (> (length api-key) 8))
                      (concat (substring api-key 0 4)
                              "..."
                              (substring api-key -4))
                    "****")))
    (format "endpoint: %s\napi-key:  %s\npayload:\n%s"
            endpoint redacted
            (json-encode payload))))

(defun org-babel-expand-body:prompt (body params)
  "Expand BODY for a prompt block by substituting :var references.
Each :var binding replaces $name in BODY with the variable's value."
  (let ((expanded body))
    (dolist (pair (org-babel--get-vars params) expanded)
      (setq expanded
            (replace-regexp-in-string
             (regexp-quote (format "$%s" (car pair)))
             (save-match-data (format "%s" (cdr pair)))
             expanded
             t t)))))

(defun org-babel-prep-session:prompt (_session _params)
  "Prepare a prompt session.
Prompt sessions are not yet supported."
  (error "ob-prompt: sessions are not yet implemented"))

;;;###autoload
(with-eval-after-load 'org
  (require 'ob-prompt))

(provide 'ob-prompt)

;;; ob-prompt.el ends here
