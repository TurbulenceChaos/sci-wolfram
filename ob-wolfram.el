;;; ob-wolfram.el --- Wolfram Mathematica repl -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025-2026 PENG
;; Created: 2025-05-20
;; Author: PENG <p.peng01@outlook.com>
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages processes tools
;; Homepage: https://github.com/TurbulenceChaos/sci-wolfram

;; This file is not part of GNU Emacs

;;; License
;;
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
;;
;; Add wolfram repl support
;;
;; Installation:
;;
;; Please check README.md.
;;
;; See https://github.com/TurbulenceChaos/sci-wolfram for more information.

;;; Code:

(require 'ob-comint)
(require 'sci-wolfram-display-images)

(defvar sci-wolfram-repl-buffer "*Wolfram REPL*")

(defvar sci-wolfram-org-babel--initiated nil)

(defvar sci-wolfram-org-babel-async--registered nil)

(defvar sci-wolfram-prompt-regexp "^In\\[[0-9]+\\]:= ")

(defun sci-wolfram-make-repl ()
  (unless (comint-check-proc sci-wolfram-repl-buffer)
    (message "Starting Wolfram REPL")
    (make-comint-in-buffer "sci-wolfram-repl" sci-wolfram-repl-buffer "wolframscript" nil "-rawterm")
    (with-current-buffer sci-wolfram-repl-buffer
      (setq-local comint-prompt-regexp sci-wolfram-prompt-regexp))
    (setq sci-wolfram-org-babel--initiated nil)
    (setq sci-wolfram-org-babel-async--registered nil)))

(defun sci-wolfram-remove-space-lines (body)
  "Remove code string space lines"
  (substring-no-properties
   (replace-regexp-in-string "\n[ \t\n]+" "\n" body)))

(defun sci-wolfram-remove-eoe (result eoe)
  "Remove EOE from result"
  (mapconcat 'identity
	     (cl-remove-if (lambda (string) (string-match-p eoe string)) result)))

(defun sci-wolfram-evaluate-session (body)
  "wolfram org-babel block execute session"
  (let* ((eoe (format "ob_comint_session_wolfram_eoe_%s" (org-id-uuid)))
	 (code (concat
		(format "%s\n" (sci-wolfram-remove-space-lines body))
		(format "WriteString[\"stdout\", \"%s\", \"\\n\"];\n" eoe)))
	 (result
	  (org-babel-comint-with-output
	      (sci-wolfram-repl-buffer eoe)
	    (comint-send-string sci-wolfram-repl-buffer code))))
    (sci-wolfram-remove-eoe result eoe)))

(defun sci-wolfram-initiate-session ()
  (unless sci-wolfram-org-babel--initiated
    (sci-wolfram-evaluate-session "WriteString[\"stdout\", \"Initiate Wolfram REPL\", \"\\n\"];\n"))
  (setq sci-wolfram-org-babel-initiated t))

(defvar sci-wolfram-async-org-block-info nil "wolfram async org block buffer and position info")

(defun sci-wolfram-async-org-block-get-info ()
  (let ((buf (current-buffer))
	(pos (point)))
    (setq sci-wolfram-async-org-block-info (cons buf pos))))

(add-hook 'org-babel-after-execute-hook 'sci-wolfram-async-org-block-get-info)

(defun sci-wolfram-clean-result (result)
  (prog1
      result
    (let ((buf (car sci-wolfram-async-org-block-info))
	  (pos (cdr sci-wolfram-async-org-block-info)))
      (run-at-time 0 nil (lambda ()
			   (with-current-buffer buf
			     (save-excursion
			       (goto-char pos)
			       (sci-wolfram-display-images))))))))

(defun sci-wolfram-org-babel-register-async ()
  (let ((buf (current-buffer)))
    (unless (and sci-wolfram-org-babel-async--registered
		 (eq buf (car sci-wolfram-async-org-block-info)))
      (org-babel-comint-async-register
       sci-wolfram-repl-buffer buf
       "ob_comint_async_wolfram_\\(start\\|end\\|file\\)_\\(.+\\)"
       'sci-wolfram-clean-result
       'org-babel-eval-read-file)
      (setq sci-wolfram-org-babel-async--registered t))))

(defun sci-wolfram-async-evaluate-session (body)
  "wolfram org-babel block async execute session"
  (sci-wolfram-org-babel-register-async)
  (let* ((uuid (org-id-uuid))
         (start (format "ob_comint_async_wolfram_start_%s" uuid))
         (end   (format "ob_comint_async_wolfram_end_%s" uuid))
         (code (concat
		(format "WriteString[\"stdout\", \"%s\", \"\\n\"];\n" start)
		(format "%s\n" (sci-wolfram-remove-space-lines body))
		(format "WriteString[\"stdout\", \"%s\", \"\\n\"];\n" end))))
    (comint-send-string sci-wolfram-repl-buffer code)
    uuid))

;;;###autoload
(defun org-babel-execute:wolfram (body params)
  "wolfram org-babel block sync/async execute session"
  (sci-wolfram-make-repl)
  (sci-wolfram-initiate-session)
  (let ((async (cdr (assq :async params))))
    (if (string-match-p "yes" async)
	(sci-wolfram-async-evaluate-session body)
      (sci-wolfram-evaluate-session body))))

(defvar org-babel-default-header-args:wolfram
  `((:session . ,sci-wolfram-repl-buffer)
    (:async . "yes")
    (:results . "value drawer")
    (:display . "text")
    (:comments . "link")
    (:eval . "never-export")
    (:exports . "both"))
  "Default header arguments for wolfram src-block in org-mode")

;;;###autoload
(defvar sci-wolfram-org-src-block-name "wolfram" "wolfram src-block name in org-mode")

;;;###autoload
(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes `(,sci-wolfram-org-src-block-name . sci-wolfram)))

(defun sci-wolfram-auto-display-images ()
  "Auto display latex or images after executing wolfram session block."
  (let* ((info (org-babel-get-src-block-info))
	 (lang (nth 0 info))
	 (params (nth 2 info))
	 (async (cdr (assq :async params))))
    (when (string= lang sci-wolfram-org-src-block-name)
      (unless (string-match-p "yes" async)
	(sci-wolfram-display-images)))))

(add-hook 'org-babel-after-execute-hook 'sci-wolfram-auto-display-images)


(provide 'ob-wolfram)
;;; ob-wolfram.el ends here
