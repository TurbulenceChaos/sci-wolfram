;;; Emacs wolfram-terminal-image setup 
;; Author: Peng Peng - 2025
;; Email: 211110103110@stu.just.edu.cn
;; GitHub: https://github.com/TurbulenceChaos/Wolfram-terminal-image

;; Load dependencies for Emacs-Jupyter
;; https://github.com/WolframResearch/WolframLanguageForJupyter
;; https://github.com/emacs-jupyter/jupyter
(add-to-list 'load-path "~/.emacs.d/lisp-site/websocket")
(require 'websocket)
(add-to-list 'load-path "~/.emacs.d/lisp-site/simple-httpd")
(require 'simple-httpd)
(add-to-list 'load-path "~/.emacs.d/lisp-site/zmq")
(require 'zmq)
(add-to-list 'load-path "~/.emacs.d/lisp-site/jupyter")
(require 'jupyter)

;; Configure Wolfram Mode
;; https://github.com/xahlee/xah-wolfram-mode
(add-to-list 'load-path "~/.emacs.d/lisp-site/xah-wolfram-mode")
(require 'xah-wolfram-mode)
(defalias 'wolfram-language-mode 'xah-wolfram-mode)

;; Configure Org-Babel for Jupyter-Wolfram-Language
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (jupyter . t)))

(setq org-babel-default-header-args:jupyter-Wolfram-Language '((:async . "yes")
                                                               (:kernel . "wolframlanguage14.1")
                                                               (:session . "jupyter-wolfram-language")
                                                               (:results . "value drawer")
                                                               (:display . "text")
                                                               (:comments . "link")
                                                               (:eval . "never-export")))

;; Ensure inline images display correctly
(setq org-babel-min-lines-for-block-output 1000)

(defun clean-wolfram-results ()
  "Clean up Wolfram Language results in org-mode.
Removes unwanted formatting while preserving necessary LaTeX formatting.
Keeps ': ' prefix only for lines starting with 'Out['."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward ":results:" nil t)
      (let ((start (point))
            (end (progn (search-forward ":end:" nil t)
                        (match-beginning 0))))
        (save-restriction
          (narrow-to-region start end)
          
          ;; First mark 'Out[' lines to preserve them
          (goto-char (point-min))
          (while (re-search-forward "^: Out\\[" nil t)
            (add-text-properties (line-beginning-position) (line-end-position)
                                 '(preserve-prefix t)))
          
          ;; Remove ': ' at beginning of lines except those marked
          (goto-char (point-min))
          (while (re-search-forward "^: " nil t)
            (unless (get-text-property (line-beginning-position) 'preserve-prefix)
              (replace-match "" nil nil)))
          
          ;; Remove text properties we added
          (remove-text-properties (point-min) (point-max) '(preserve-prefix nil))
          
          ;; Remove '> ' at beginning of lines 
          (goto-char (point-min))
          (while (re-search-forward "^> " nil t)
            (replace-match "  " nil nil))
          
          ;; Remove SINGLE backslashes at end of lines (not double backslashes)
          (goto-char (point-min))
          (while (re-search-forward "\\([^\\]\\)\\\\\\s-*$" nil t)
            (replace-match "\\1" nil nil))
          
          ;; Remove blank lines
          (goto-char (point-min))
          (while (re-search-forward "\n\\s-*\n" nil t)
            (replace-match "\n" nil nil))
          
          ;; Create a new line after :results: if needed
          (goto-char (point-min))
          (unless (looking-at "\n")
            (insert "\n")))))))

(add-hook 'org-babel-after-execute-hook
          '(lambda ()
             (clean-wolfram-results)
             (org-latex-preview)
             (org-display-inline-images)))


(provide 'wolfram-terminal-image)