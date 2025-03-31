;;; llm-vc-commit.el --- LLM-based commit message generator  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/llm-vc-commit.el
;; Package-Requires: ((emacs "27.1") (content-quoter "0.2"))
;; Keywords: convenience, vc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A package that uses `log-edit-generate-changelog-from-diff' plus
;; `llm-chat-streaming' to produce commit messages.

;; Usage:
;;
;; (use-package llm-vc-commit
;;   :ensure (:host github :repo "ultronozm/llm-vc-commit.el" :depth nil)
;;   :after log-edit
;;   :bind (:map log-edit-mode-map
;;               ("C-c C-r" . llm-vc-commit-generate-message))
;;   :config
;;   (setq llm-vc-commit-contribute-file
;;         (expand-file-name "CONTRIBUTE" "~/gnu-emacs/"))
;;   (require 'llm-claude)
;;   (require 'content-quoter) ; optional
;;   (setq llm-vc-commit-model
;;         (make-llm-claude
;;          :key (exec-path-from-shell-getenv "ANTHROPIC_KEY")
;;          :chat-model "claude-3-7-sonnet-20250219")))
;;
;; For some features, requires the `content-quoter' package
;; (https://github.com/ultronozm/content-quoter.el).

;;; Code:

(require 'log-edit)
(require 'llm)

(defgroup llm-vc-commit nil
  "LLM-based commit message generation."
  :group 'tools
  :prefix "llm-vc-commit-")

(defcustom llm-vc-commit-model nil
  "LLM provider object to use when generating commit messages.."
  :type '(choice (const :tag "None" nil)
                 (symbol :tag "LLM provider"))
  :group 'llm-vc-commit)

(defcustom llm-vc-commit-contribute-file nil
  "Path to Emacs CONTRIBUTE file.
This file contains guidelines for commit messages."
  :type 'file
  :group 'llm-vc-commit)

(defcustom llm-vc-commit-prompt-addendum
  "Follow these EXACT formatting rules:
1. Start with a concise, specific summary line (max 50 chars)
2. For ChangeLog entries, list each file with a comma before the asterisk (like \"* filename.el\")
3. After the initial file description, put each function/variable on its own line
4. Each variable/function should be properly indented
5. ALWAYS use two spaces between sentences in all descriptions
6. Keep line length under 63 chars (max 78 chars absolute limit)
5. ALWAYS use two spaces between sentences in all descriptions
6. Format the message exactly like this example:

Fix parsing issue in regexp handling.  Use two spaces here.  This uses two spaces after periods.

* some-file.el (function-name): Description of what changed.  Note the two spaces here.  Note the two spaces here.
(another-function): What changed here.  Always double-space between sentences.
(some-variable): How this variable changed.

* another-file.el (some-function): What changed.  Two spaces after periods.

Output only the commit message with no additional commentary."
  "Additional commit message guidance.
This should provide specific formatting instructions."
  :type 'string
  :group 'llm-vc-commit)

(defun llm-vc-commit--collect-context (use-visible-buffers)
  "Collect context for commit message generation.
When USE-VISIBLE-BUFFERS is non-nil, collect context from all visible buffers.
Otherwise, just collect from the *vc-diff* buffer."
  (if use-visible-buffers
      ;; Use visible buffers + diff buffer
      (let ((visible-buffers (and (fboundp 'content-quoter--collect-visible-buffers)
                                  (content-quoter--collect-visible-buffers)))
            (diff-buf (get-buffer "*vc-diff*")))
        (when (and visible-buffers diff-buf (not (member diff-buf visible-buffers)))
          (push diff-buf visible-buffers))
        (if (and visible-buffers (fboundp 'content-quoter--get-content-plists))
            (content-quoter--format-plists-to-string
             (content-quoter--get-content-plists visible-buffers)
             content-quoter-wrapper)
          ""))
    ;; Only use the diff buffer
    (let ((diff-buf (get-buffer "*vc-diff*")))
      (if (and diff-buf (fboundp 'content-quoter--get-content-plists))
          (content-quoter--format-plists-to-string
           (content-quoter--get-content-plists (list diff-buf))
           content-quoter-wrapper)
        ""))))

(defun llm-vc-commit-extract-commit-messages-section (file-path)
  "Extract the `Commit messages' section from the specified FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (let (section-start section-end)
      (when (re-search-forward "^\\*\\* Commit messages$" nil t)
        (setq section-start (point))
        (if (re-search-forward "^\\*\\* " nil t)
            (setq section-end (match-beginning 0))
          (setq section-end (point-max)))
        (buffer-substring-no-properties section-start section-end)))))

(defun llm-vc-commit-get-commit-guidelines ()
  "Get the commit message guidelines from the CONTRIBUTE file.
Returns the content of the `Commit messages' section as a string, or nil
if the section or file cannot be found."
  (when (and llm-vc-commit-contribute-file
             (file-exists-p llm-vc-commit-contribute-file))
    (llm-vc-commit-extract-commit-messages-section llm-vc-commit-contribute-file)))

(defun llm-vc-commit-format-message ()
  "Format the generated commit message according to Emacs conventions.
This handles proper line breaks for ChangeLog-style entries."
  (save-excursion
    (goto-char (point-min))
    ;; Fill the summary line
    (when (re-search-forward "^[^*\n].*$" nil t)
      (fill-region (line-beginning-position) (line-end-position)))

    (while (re-search-forward "^\\(,\\* .*?\\):" nil t)
      (let ((desc-start (match-end 0)))
        ;; Put each function/variable on its own line with proper indentation
        (while (re-search-forward "(\\([^)]+\\))" (line-end-position) t)
          (unless (= (match-beginning 0) desc-start) ; Skip if it's the first one
            (replace-match "\n\\1" t nil)))

        ;; Fill each entry line properly
        (let ((end (line-end-position)))
          (goto-char desc-start)
          (while (< (point) end)
            (when (looking-at "\\s-*$") (delete-horizontal-space))
            (unless (eolp)
              (fill-region (point) (line-end-position) nil t))
            (forward-line 1)))))))

;;;###autoload
(defun llm-vc-commit-generate-message (&optional arg)
  "Generate or improve a commit message using an LLM.

1. Insert scaffolding from `log-edit-generate-changelog-from-diff'.

2. Gather context from *vc-diff* buffer by default.  With prefix ARG,
   also gather context from all visible buffers provided that the
   `content-quoter' package is loaded.

3. Combine everything into a prompt and send to `llm-vc-commit-model' via
`llm-chat-streaming`."
  (interactive "P")
  (unless llm-vc-commit-model
    (user-error "No LLM model set.  Set `llm-vc-commit-model` to something like (make-llm-openai :chat-model \"...\")"))

  (save-restriction
    (narrow-to-region (point) (point-max))
    (log-edit-generate-changelog-from-diff)
    (let* ((scaffold (buffer-string))
           (context-text (llm-vc-commit--collect-context arg))
           (guidelines (or (llm-vc-commit-get-commit-guidelines)
                           "Follow standard Emacs commit message conventions."))
           (prompt (format "You are a helpful assistant that writes commit messages for Emacs.

Here are the official Emacs commit message guidelines:
%s

%s

Context from code changes:
%s

Scaffold generated by Emacs:
%s

Please generate a complete, properly formatted commit message based on the scaffold and context."
                           guidelines
                           (if (string-empty-p llm-vc-commit-prompt-addendum)
                               ""
                             (concat "IMPORTANT: " llm-vc-commit-prompt-addendum))
                           context-text
                           scaffold)))
      (delete-region (point-min) (point-max))
      (llm-chat-streaming-to-point
       llm-vc-commit-model
       (llm-make-chat-prompt prompt)
       (current-buffer)
       (point)
       (lambda () (llm-vc-commit-format-message))))))

(provide 'llm-vc-commit)

;;; llm-vc-commit.el ends here
