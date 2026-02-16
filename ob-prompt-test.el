;;; ob-prompt-test.el --- Tests for ob-prompt -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pablo Munoz

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

;; ERT test suite for ob-prompt.  Run with:
;;   emacs -batch -L . -l ob-prompt-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'ob-prompt)

;;; --- Building Messages ---

(ert-deftest ob-prompt-messages-body-only ()
  "A bare prompt produces a single user message."
  (let ((msgs (ob-prompt--build-messages "hello")))
    (should (= 1 (length msgs)))
    (should (equal "user" (alist-get 'role (aref msgs 0))))
    (should (equal "hello" (alist-get 'content (aref msgs 0))))))

(ert-deftest ob-prompt-messages-with-system ()
  "A system prompt prepends a system-role message."
  (let ((msgs (ob-prompt--build-messages "hello" "be concise")))
    (should (= 2 (length msgs)))
    (should (equal "system" (alist-get 'role (aref msgs 0))))
    (should (equal "be concise" (alist-get 'content (aref msgs 0))))
    (should (equal "user" (alist-get 'role (aref msgs 1))))
    (should (equal "hello" (alist-get 'content (aref msgs 1))))))

(ert-deftest ob-prompt-messages-with-preamble ()
  "Preamble text wraps the body in <context> tags."
  (let* ((msgs (ob-prompt--build-messages "summarize" nil "doc text"))
         (content (alist-get 'content (aref msgs 0))))
    (should (= 1 (length msgs)))
    (should (string-match-p "<context>" content))
    (should (string-match-p "doc text" content))
    (should (string-match-p "summarize" content))))

(ert-deftest ob-prompt-messages-system-and-preamble ()
  "System + preamble + body all compose correctly."
  (let ((msgs (ob-prompt--build-messages "go" "be brief" "ctx")))
    (should (= 2 (length msgs)))
    (should (equal "system" (alist-get 'role (aref msgs 0))))
    (should (string-match-p "<context>" (alist-get 'content (aref msgs 1))))
    (should (string-match-p "go" (alist-get 'content (aref msgs 1))))))

(ert-deftest ob-prompt-messages-nil-system-ignored ()
  "Passing nil for system does not add a system message."
  (let ((msgs (ob-prompt--build-messages "hi" nil)))
    (should (= 1 (length msgs)))
    (should (equal "user" (alist-get 'role (aref msgs 0))))))

;;; --- Building Payload ---

(ert-deftest ob-prompt-payload-basic ()
  "Payload includes model, messages, and max tokens."
  (let* ((msgs (ob-prompt--build-messages "hi"))
         (payload (ob-prompt--build-payload "gpt-4" msgs)))
    (should (equal "gpt-4" (alist-get "model" payload nil nil #'equal)))
    (should (equal 16384 (alist-get "max_completion_tokens" payload nil nil #'equal)))
    (should (equal msgs (alist-get "messages" payload nil nil #'equal)))))

;;; --- Parsing Response ---

(ert-deftest ob-prompt-parse-valid-response ()
  "Extracts content from a well-formed response."
  (let ((json "{\"choices\":[{\"message\":{\"content\":\"hello world\"}}]}"))
    (should (equal "hello world" (ob-prompt--parse-response json)))))

(ert-deftest ob-prompt-parse-api-error ()
  "Signals user-error on API error response."
  (let ((json "{\"error\":{\"message\":\"rate limited\"}}"))
    (should-error (ob-prompt--parse-response json) :type 'user-error)))

(ert-deftest ob-prompt-parse-malformed-json ()
  "Signals user-error on unparseable JSON."
  (should-error (ob-prompt--parse-response "not json") :type 'user-error))

(ert-deftest ob-prompt-parse-unexpected-structure ()
  "Signals user-error when choices array is missing."
  (let ((json "{\"id\":\"123\"}"))
    (should-error (ob-prompt--parse-response json) :type 'user-error)))

;;; --- Sending Request ---

(ert-deftest ob-prompt-request-sends-auth-header ()
  "Request includes Bearer token in Authorization header."
  (let (captured-headers)
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (_url &rest _)
                 (setq captured-headers url-request-extra-headers)
                 (let ((buf (generate-new-buffer " *test-http*")))
                   (with-current-buffer buf
                     (insert "HTTP/1.1 200 OK\r\n\r\n"
                             "{\"choices\":[{\"message\":{\"content\":\"ok\"}}]}"))
                   buf))))
      (ob-prompt--request "https://example.com" "sk-test" '(("model" . "x")))
      (should (equal "Bearer sk-test"
                     (alist-get "Authorization" captured-headers
                                nil nil #'equal))))))

;;; --- ASCII-safe JSON Encoding ---

(ert-deftest ob-prompt-json-encode-ascii-escapes-non-ascii ()
  "Non-ASCII characters are escaped to \\uNNNN."
  (let ((result (ob-prompt--json-encode-ascii '(("text" . "hello — world")))))
    (should-not (string-match-p "[^\x00-\x7f]" result))
    (should (string-match-p "\\\\u2014" result))))

;;; --- Debug Formatting ---

(ert-deftest ob-prompt-debug-redacts-key ()
  "Debug output masks the middle of the API key."
  (let ((output (ob-prompt--format-debug
                 "https://x.com" "sk-1234567890abcdef"
                 '(("model" . "test")))))
    (should (string-match-p "sk-1" output))
    (should (string-match-p "cdef" output))
    (should-not (string-match-p "1234567890abcde" output))))

(ert-deftest ob-prompt-debug-short-key ()
  "Short API keys are fully redacted."
  (let ((output (ob-prompt--format-debug "https://x.com" "short" '())))
    (should (string-match-p "\\*\\*\\*\\*" output))
    (should-not (string-match-p "short" output))))

;;; --- Variable Expansion ---

(ert-deftest ob-prompt-expand-body-substitutes-vars ()
  "Variables from :var are substituted into the body."
  (let* ((params '((:var . (code . "def foo(): pass"))))
         (result (org-babel-expand-body:prompt "Review: $code" params)))
    (should (equal "Review: def foo(): pass" result))))

(ert-deftest ob-prompt-expand-body-no-vars ()
  "Body is returned unchanged when there are no :var bindings."
  (let ((result (org-babel-expand-body:prompt "plain text" nil)))
    (should (equal "plain text" result))))

;;; --- Full Execute Round-trip ---

(ert-deftest ob-prompt-execute-round-trip ()
  "Full execute path with mocked HTTP returns parsed content."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _)
               (let ((buf (generate-new-buffer " *test-http*")))
                 (with-current-buffer buf
                   (insert "HTTP/1.1 200 OK\r\n\r\n"
                           "{\"choices\":[{\"message\":{\"content\":\"42\"}}]}"))
                 buf)))
            ((symbol-function 'org-element-at-point)
             (lambda () '(src-block (:begin 1)))))
    (let ((params '((:model . "test-model")
                    (:endpoint . "https://example.com")
                    (:api-key . "sk-test1234test")
                    (:result-params . ("raw")))))
      (should (equal "42" (org-babel-execute:prompt "question" params))))))

(ert-deftest ob-prompt-execute-debug-skips-http ()
  "With :debug set, returns formatted request without calling API."
  (let ((params '((:model . "test-model")
                  (:endpoint . "https://example.com")
                  (:api-key . "sk-test1234test")
                  (:debug . "yes")
                  (:result-params . ("raw")))))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _) (error "should not be called")))
              ((symbol-function 'org-element-at-point)
               (lambda () '(src-block (:begin 1)))))
      (let ((result (org-babel-execute:prompt "hello" params)))
        (should (string-match-p "endpoint:" result))
        (should (string-match-p "test-model" result))))))

(ert-deftest ob-prompt-preamble-no-disables-context ()
  "Setting :preamble to \"no\" should not include buffer context."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _)
               (let ((buf (generate-new-buffer " *test-http*")))
                 (with-current-buffer buf
                   (insert "HTTP/1.1 200 OK\r\n\r\n"
                           "{\"choices\":[{\"message\":{\"content\":\"ok\"}}]}"))
                 buf)))
            ((symbol-function 'org-element-at-point)
             (lambda () '(src-block (:begin 1)))))
    (let ((params '((:model . "test-model")
                    (:endpoint . "https://example.com")
                    (:api-key . "sk-test1234test")
                    (:preamble . "no")
                    (:debug . "yes")
                    (:result-params . ("raw")))))
      (let ((result (org-babel-execute:prompt "hello" params)))
        (should-not (string-match-p "<context>" result))))))

(ert-deftest ob-prompt-debug-no-calls-api ()
  "Setting :debug to \"no\" should call the API, not return debug output."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _)
               (let ((buf (generate-new-buffer " *test-http*")))
                 (with-current-buffer buf
                   (insert "HTTP/1.1 200 OK\r\n\r\n"
                           "{\"choices\":[{\"message\":{\"content\":\"api-response\"}}]}"))
                 buf)))
            ((symbol-function 'org-element-at-point)
             (lambda () '(src-block (:begin 1)))))
    (let ((params '((:model . "test-model")
                    (:endpoint . "https://example.com")
                    (:api-key . "sk-test1234test")
                    (:debug . "no")
                    (:result-params . ("raw")))))
      (let ((result (org-babel-execute:prompt "hello" params)))
        (should (equal "api-response" result))))))

;;; --- UTF-8 Response Decoding ---

(ert-deftest ob-prompt-request-decodes-utf8-response ()
  "Response body is decoded from UTF-8 to proper multibyte text."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _)
               (let ((buf (generate-new-buffer " *test-http*")))
                 (with-current-buffer buf
                   (set-buffer-multibyte nil)
                   (insert "HTTP/1.1 200 OK\r\n\r\n")
                   (insert (encode-coding-string
                            "{\"choices\":[{\"message\":{\"content\":\"hello \xe2\x80\x94 world\"}}]}"
                            'utf-8)))
                 buf))))
    (let ((body (ob-prompt--request "https://example.com" "sk-test" '(("model" . "x")))))
      (should (string-match-p "\u2014" (ob-prompt--parse-response body))))))

(provide 'ob-prompt-test)

;;; ob-prompt-test.el ends here
