# Wolfram-terminal-image

**Author:** Peng Peng  \
**Email:** [211110103110@stu.just.edu.cn](mailto:211110103110@stu.just.edu.cn)  \
**GitHub:** [TurbulenceChaos](https://github.com/TurbulenceChaos)

---

## Table of Contents

- [Wolfram-terminal-image](#wolfram-terminal-image)
  - [Table of Contents](#table-of-contents)
  - [1. Prerequisites](#1-prerequisites)
  - [2. Getting Started](#2-getting-started)
    - [2.1 For VS Code](#21-for-vs-code)
    - [2.2 For Emacs](#22-for-emacs)
      - [2.2.1 Emacs Configuration](#221-emacs-configuration)
      - [2.2.2 Executing Jupyter Wolfram Language Code in Org-Mode](#222-executing-jupyter-wolfram-language-code-in-org-mode)
  - [3. Reference](#3-reference)

---

## 1. Prerequisites
- **`wolframscript`** – Required for executing Wolfram Language scripts.
- **`imgcat`** – Install using `pip install imgcat` to display images in the vscode terminal.
- **`wolframplayer`** (optional) – Needed for playing `.cdf` files.

## 2. Getting Started

### 2.1 For VS Code

1. Enable **`Terminal > Integrated: Enable Images`** and **`Terminal > Integrated: GPU Acceleration`** in VS Code settings.

   ![Enable images in VS Code terminal](Images/vscode-terminal-enable-images.png)

2. Install the official **Wolfram Language extension**.

   ![Install Wolfram extension](Images/vscode-official-wolfram-extension.png)

3. Run the `Test.wl` script to verify your setup.

   Demo: [wolfram-test.gif](https://github.com/TurbulenceChaos/Wolfram-terminal-image/blob/main/Images/wolfram-test.gif)  

   ![Wolfram script test](Images/wolfram-test.gif)

### 2.2 For Emacs
For Emacs Org mode, formulas are converted into LaTeX fragments, making them easy to paste into Word or LaTeX, while plots are exported as PNG images. 
You can place the cursor on a formula and press `C-c C-x C-l` to toggle LaTeX fragments. 

Demo: [Test-emacs-jupyter-wolfram-language.gif](https://github.com/TurbulenceChaos/Wolfram-terminal-image/blob/main/Images/Test-emacs-jupyter-wolfram-language.gif)  

![Jupyter-Wolfram output](Images/Test-emacs-jupyter-wolfram-language.gif)

#### 2.2.1 Emacs Configuration
Ensure you have the necessary dependencies installed for Emacs-Jupyter integration.

- [WolframLanguageForJupyter](https://github.com/WolframResearch/WolframLanguageForJupyter)
- [emacs-jupyter](https://github.com/emacs-jupyter/jupyter)
- [xah-wolfram-mode](https://github.com/xahlee/xah-wolfram-mode)

You can find the Emacs setup file [wolfram-terminal-image.el](Test/wolfram-terminal-image.el) and the test file [Test.org](Test/Test.org) in the Test folder.

```emacs-lisp
;; Load dependencies for Emacs-Jupyter
;; https://github.com/WolframResearch/WolframLanguageForJupyter
;; https://github.com/emacs-jupyter/jupyter
(add-to-list 'load-path "~/.emacs.d/lisp-site/jupyter/.eldev/30.1/packages/websocket-1.15")
(require 'websocket)

(add-to-list 'load-path "~/.emacs.d/lisp-site/jupyter/.eldev/30.1/packages/simple-httpd-1.5.1")
(require 'simple-httpd)

(add-to-list 'load-path "~/.emacs.d/lisp-site/jupyter/.eldev/30.1/packages/zmq-1.0.2")
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
```

#### 2.2.2 Executing Jupyter Wolfram Language Code in Org-Mode
First, import the [WolframTerminalImage.wl](https://github.com/TurbulenceChaos/Wolfram-terminal-image/blob/main/WolframTerminalImage.wl) package.

```wolfram
#+name: Import-Wolfram-terminal-image-package
#+begin_src jupyter-Wolfram-Language :results silent
  Get["/path/to/WolframTerminalImage.wl"]

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

Next, test Jupyter-Wolfram-Language by solving a PDE and visualizing the solution with a plot.

```wolfram
#+name: Wolfram-test
#+begin_src jupyter-Wolfram-Language
  sol1 = DSolve[{D[y[x, t], t] + 2 D[y[x, t], x] == Sin[x], y[0, t] == 
     Cos[t]}, y[x, t], {x, t}]

  sol2 = sol1[[1, 1, 2]]

  Plot3D[sol2, {x, -10, 10}, {t, -5, 5}]  

  MatrixForm[Array[Subscript[a, ##] &, {2, 2, 2}]]
#+end_src
```

For my Emacs configuration, check out: [SCI-emacs](https://github.com/TurbulenceChaos/SCI-emacs).

## 3. Reference

1. [Wolfram Community Discussion](https://community.wolfram.com/groups/-/m/t/2864001)
2. [Mathematica Stack Exchange](https://mathematica.stackexchange.com/questions/258273/how-to-set-up-a-plot-viewer-for-wolfram-engine)
