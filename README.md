# Wolfram-terminal-image

**Author:** Peng Peng  \
**Email:** [211110103110@stu.just.edu.cn](mailto:211110103110@stu.just.edu.cn)  \
**GitHub:** [TurbulenceChaos/Wolfram-terminal-image](https://github.com/TurbulenceChaos/Wolfram-terminal-image)

---

## Table of Contents
- [Wolfram-terminal-image](#wolfram-terminal-image)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Prerequisites](#prerequisites)
  - [For VS Code](#for-vs-code)
  - [For Emacs Org-Mode](#for-emacs-org-mode)
    - [Emacs configuration](#emacs-configuration)
    - [Executing jupyter-Wolfram-Language code in org-mode](#executing-jupyter-wolfram-language-code-in-org-mode)
  - [Reference](#reference)

---
## Introduction
Display wolfram script graphics in vscode terminal and emacs org-mode using [Wolfram-terminal-image](https://github.com/TurbulenceChaos/Wolfram-terminal-image) package.

Demo: [wolfram-test.gif](https://github.com/TurbulenceChaos/Wolfram-terminal-image/blob/main/Images/wolfram-test.gif)  

![Wolfram script test](Images/wolfram-test.gif)

Demo: [Test-emacs-jupyter-wolfram-language.gif](https://github.com/TurbulenceChaos/Wolfram-terminal-image/blob/main/Images/Test-emacs-jupyter-wolfram-language.gif)  

![Jupyter-Wolfram output](Images/Test-emacs-jupyter-wolfram-language.gif)

## Prerequisites
- `wolframscript`, `Wolfram Engine` or `Mathematica` – Required for executing wolfram language scripts, which can be downloaded from https://www.wolfram.com/download-center/index.php.en 
- `imgcat` – Install using `pip install imgcat` to display images in vscode terminal.
- `Wolfram Player` (optional) – Needed for viewing and interacting with `.cdf` files.

## For VS Code

1. Enable **`Terminal > Integrated: Enable Images`** and **`Terminal > Integrated: GPU Acceleration`** in vscode settings, and make sure your system is using a discrete graphics card.

   ![Enable images in VS Code terminal](Images/vscode-terminal-enable-images.png)

2. Install the official [wolfram language extension](https://github.com/WolframResearch/vscode-wolfram) from vscode extension marketplace.

   ![Install Wolfram extension](Images/vscode-official-wolfram-extension.png)

3. Run [Test.wl](Test/Test.wl) script to verify your setup.
   ```Mathematica
   (* Get["https://raw.githubusercontent.com/TurbulenceChaos/Wolfram-terminal-image/refs/heads/main/WolframTerminalImage.wl"]; *)

   Get["/path/to/WolframTerminalImage.wl"];

   sol1 = DSolve[{D[y[x, t], t] + 2 D[y[x, t], x] == Sin[x], y[0, t] == 
     Cos[t]}, y[x, t], {x, t}]

   sol2 = sol1[[1, 1, 2]]

   Plot3D[sol2, {x, -10, 10}, {t, -5, 5}]
   ```

## For Emacs Org-Mode
For emacs org-mode, formulas are converted into LaTeX fragments by default, making it easy to paste them into Microsoft Word or LaTeX documents.

You can place the cursor on a formula and press `C-c C-x C-l` to toggle LaTeX fragments preview.

### Emacs configuration
Ensure you have the necessary dependencies installed.
- [Jupyter](https://jupyter.org/install)
- [WolframLanguageForJupyter](https://github.com/WolframResearch/WolframLanguageForJupyter)
- [emacs-jupyter](https://github.com/emacs-jupyter/jupyter)
- [xah-wolfram-mode](https://github.com/xahlee/xah-wolfram-mode)
- LaTeX (optional) - Install using `sudo apt install texlive-full` to preview latex in emacs org-mode; otherwise you should set `wolframTerminalFormulaType="image"` in wolfram scripts to convert formulas to `.png` files, and set `(setq wolfram-terminal-formula-type=latex nil)` in emacs-lisp.

You can find the configuration file [wolfram-terminal-image.el](Test/wolfram-terminal-image.el) and test file [Test.org](Test/Test.org) in the _Test_ folder.

```emacs-lisp
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; 1. Install emacs-jupyter
;; https://github.com/emacs-jupyter/jupyter
(package-install 'jupyter)
(require 'jupyter)

;; 2. Install wolfram-mode for syntax highlight, code format and completion
;; https://github.com/xahlee/xah-wolfram-mode
;; For emacs 29+, you can use `package-vc-install` to install packages directly from github;
;; otherwise you can manually download the package and add it to `load-path`
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/xah-wolfram-mode")
(package-vc-install "https://github.com/xahlee/xah-wolfram-mode.git")
(require 'xah-wolfram-mode)
(defalias 'wolfram-language-mode 'xah-wolfram-mode)

;; 3. Add jupyter-Wolfram-Language to org-babel
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

;; 4. Clean jupyter-wolfram-language results
(setq org-babel-min-lines-for-block-output 1000)

(defcustom wolfram-terminal-formula-type=latex t
  "A boolean option.  When set to t, wolfram-terminal-formula-type='latex';
when set to nil, wolfram-terminal-formula-type='image'."
  :type 'boolean
  :group 'wolfram-terminal-image)

(defun clean-jupyter-wolfram-language-results ()
  "Clean up jupyter-Wolfram-Language results."
  (let ((result-beg (org-babel-where-is-src-block-result)))
    (save-excursion
      (when (and result-beg
		 (goto-char result-beg))
	(let ((beg (re-search-forward "^:results:" nil t))
	      (end   (re-search-forward "^:end:" nil t)))
	  (save-restriction
	    (narrow-to-region beg end)
	    ;; Remove ': ' at beginning
	    (goto-char (point-min))
	    (while (re-search-forward "^: " nil t)
	      (replace-match "" nil nil))

	    ;; Change 'Out[number]' to ': Out[number]'
	    (goto-char (point-min))
	    (while (re-search-forward "^Out\\[\\([0-9]+\\)\\]" nil t)
	      (replace-match ": Out[\\1]" nil nil))

	    (when wolfram-terminal-formula-type=latex
	      (goto-char (point-min))
	      (let ((latex-beg 0) (latex-end 0))
		(while (setq latex-beg (re-search-forward "^\\\\begin{equation\\*}" nil t))
		  (setq latex-end (re-search-forward "^\\\\end{equation\\*}" nil t))
		  (narrow-to-region latex-beg latex-end)
		  ;; Remove blank lines
		  (goto-char (point-min))
		  (while (re-search-forward "\n\\s-*\n" nil t)
		    (replace-match "\n" nil nil))
		  
		  ;; Remove '>' at beginning
		  (goto-char (point-min))
		  (while (re-search-forward "^> " nil t)
		    (replace-match " " nil nil))

		  ;; Remove '\' at end
		  (goto-char (point-min))
		  (while (re-search-forward "\\([^\\]\\)\\\\\\s-*$" nil t)
		    (replace-match "\\1" nil nil)))))))))))

;; Display inline images and latex fragments in org-babel result
;; https://github.com/doomemacs/doomemacs/blob/303dd28db808b42a2397c0f4b9fdd71e606026ff/modules/lang/org/config.el#L297
(defmacro +org-define-babel-result-display-fn (name action doc)
  "Define a function to display elements in org-babel result.
NAME is the function name suffix.
ACTION is the display function to call.
DOC is the docstring."
  `(defun ,(intern (format "+org-redisplay-%s-in-babel-result-h" name)) ()
     ,doc
     (unless (or
              ;; ...but not while Emacs is exporting an org buffer
              (bound-and-true-p org-export-current-backend)
              ;; ...and not while tangling org buffers
              (string-match-p "^ \\*temp" (buffer-name)))
       (save-excursion
         (let* ((beg (org-babel-where-is-src-block-result))
                (end (progn (goto-char beg) (forward-line) (org-babel-result-end))))
           (save-restriction
             (narrow-to-region (min beg end) (max beg end))
             ,action))))))

(+org-define-babel-result-display-fn
 "latex-fragments"
 (org-latex-preview)
 "Redisplay latex fragments after executing org-block.")

(+org-define-babel-result-display-fn
 "inline-images"
 (org-display-inline-images)
 "Redisplay inline images after executing org-block.")

(defun org-display-images-in-babel-result ()
  "Display org-sliced-images after executing org block."
  (when (org-babel-where-is-src-block-result)
    (let ((lang (org-element-property :language (org-element-at-point))))
      (when (string= lang "jupyter-Wolfram-Language")
	(clean-jupyter-wolfram-language-results)
	(when wolfram-terminal-formula-type=latex
	  (+org-redisplay-latex-fragments-in-babel-result-h))))
    (+org-redisplay-inline-images-in-babel-result-h)))

(add-hook 'org-babel-after-execute-hook #'org-display-images-in-babel-result)
```

### Executing jupyter-Wolfram-Language code in org-mode
First, import the [WolframTerminalImage.wl](https://github.com/TurbulenceChaos/Wolfram-terminal-image/blob/main/WolframTerminalImage.wl) package.

```Mathematica
#+name: Import-Wolfram-terminal-image-package
#+begin_src jupyter-Wolfram-Language :results silent
  (* Get["https://raw.githubusercontent.com/TurbulenceChaos/Wolfram-terminal-image/refs/heads/main/WolframTerminalImage.wl"]; *)
  
  Get["~/.emacs.d/site-lisp/Wolfram-terminal-image/WolframTerminalImage.wl"];
  
  (* Specify the terminal type for Wolfram terminal images (options: "vscode", "emacs") *)

  wolframTerminalType = "emacs";

  (* Set the resolution (in DPI) for Wolfram terminal images *)

  wolframTerminalImageResolution = 150;

  (* Specify the formula type for emacs (options: "latex", "image") *)

  wolframTerminalFormulaType = "latex";

  (* Enable ("yes") or disable ("no") playback of Wolfram terminal CDF files *)

  wolframTerminalPlay = "no";

  (* Specify the player application for Wolfram terminal CDF files *)

  (* Options: "/path/to/wolframplayer" for Linux or WSL2, "/path/to/wolframplayer.exe" for Windows or WSL2 *)

  wolframTerminalPlayer = "wolframplayer";

  (* To restore `$Post` to its original state, simply execute "$Post=." *)
#+end_src
```

Next, test jupyter-Wolfram-Language by solving a PDE and visualizing the solution with a plot.

```Mathematica
#+name: Wolfram-test
#+begin_src jupyter-Wolfram-Language
  sol1 = DSolve[{D[y[x, t], t] + 2 D[y[x, t], x] == Sin[x], y[0, t] == 
     Cos[t]}, y[x, t], {x, t}]

  sol2 = sol1[[1, 1, 2]]

  Plot3D[sol2, {x, -10, 10}, {t, -5, 5}]

  MatrixForm[Array[Subscript[a, ##] &, {2, 2, 2}]]
#+end_src
```

For my emacs configuration, check out: [Sci-Emacs](https://github.com/TurbulenceChaos/Sci-Emacs).

## Reference

1. [Wolfram Community Discussion](https://community.wolfram.com/groups/-/m/t/2864001)
2. [Mathematica Stack Exchange](https://mathematica.stackexchange.com/questions/258273/how-to-set-up-a-plot-viewer-for-wolfram-engine)
