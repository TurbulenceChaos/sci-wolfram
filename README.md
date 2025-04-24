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
    - [Executing jupyter-wolfram-language code](#executing-jupyter-wolfram-language-code)
  - [Reference](#reference)

---
## Introduction
Display wolfram script graphics in vscode terminal and emacs org-mode using [Wolfram-terminal-image](https://github.com/TurbulenceChaos/Wolfram-terminal-image) package.

## Prerequisites
- **`wolframscript`**, **`Wolfram Engine`** or **`Mathematica`** – Required for executing wolfram language scripts, which can be downloaded from https://www.wolfram.com/download-center/index.php.en 
- **`imgcat`** – Install using `pip install imgcat` to display images in vscode terminal.
- **`Wolfram Player`** (optional) – Needed for viewing and interacting with `.cdf` files.

## For VS Code

1. Enable **`Terminal > Integrated: Enable Images`** and **`Terminal > Integrated: GPU Acceleration`** in VS Code settings. Make sure your system is using a discrete graphics card.

   ![Enable images in VS Code terminal](Images/vscode-terminal-enable-images.png)

2. Install the official [wolfram language extension](https://github.com/WolframResearch/vscode-wolfram) from vscode extension marketplace.

   ![Install Wolfram extension](Images/vscode-official-wolfram-extension.png)

3. Run [Test.wl](Test/Test.wl) script to verify your setup.
   ```Mathematica
   Get["https://raw.githubusercontent.com/TurbulenceChaos/Wolfram-terminal-image/refs/heads/main/WolframTerminalImage.wl"];

   sol1 = DSolve[{D[y[x, t], t] + 2 D[y[x, t], x] == Sin[x], y[0, t] == 
     Cos[t]}, y[x, t], {x, t}]

   sol2 = sol1[[1, 1, 2]]

   Plot3D[sol2, {x, -10, 10}, {t, -5, 5}]
   ```

   Demo: [wolfram-test.gif](https://github.com/TurbulenceChaos/Wolfram-terminal-image/blob/main/Images/wolfram-test.gif)  

   ![Wolfram script test](Images/wolfram-test.gif)

## For Emacs Org-Mode
For emacs org-mode, formulas are converted into LaTeX fragments by default, making them easy to paste into Microsoft Word or LaTeX, while plots are exported as PNG images. 

You can place the cursor on a formula and press `C-c C-x C-l` to toggle LaTeX fragments preview. 

Demo: [Test-emacs-jupyter-wolfram-language.gif](https://github.com/TurbulenceChaos/Wolfram-terminal-image/blob/main/Images/Test-emacs-jupyter-wolfram-language.gif)  

![Jupyter-Wolfram output](Images/Test-emacs-jupyter-wolfram-language.gif)

### Emacs configuration
Ensure you have the necessary dependencies installed.

- [WolframLanguageForJupyter](https://github.com/WolframResearch/WolframLanguageForJupyter)
- [emacs-jupyter](https://github.com/emacs-jupyter/jupyter)
- [xah-wolfram-mode](https://github.com/xahlee/xah-wolfram-mode)

You can find [wolfram-terminal-image.el](Test/wolfram-terminal-image.el) and [Test.org](Test/Test.org) in Test folder.

```emacs-lisp
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; 1. Add wolfram language to jupyter
;; https://github.com/WolframResearch/WolframLanguageForJupyter

;; 2. Install emacs-jupyter
;; https://github.com/emacs-jupyter/jupyter
(package-install 'jupyter)

;; 3. Install wolfram-mode
;; https://github.com/xahlee/xah-wolfram-mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/xah-wolfram-mode")
(require 'xah-wolfram-mode)
(defalias 'wolfram-language-mode 'xah-wolfram-mode)

;; 4. Add jupyter-Wolfram-Language to org-babel
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

;; 5. Clean jupyter-wolfram-language results
(setq org-babel-min-lines-for-block-output 1000)

(defun clean-jupyter-wolfram-language-results ()
  "Clean jupyter-Wolfram-Language results in org-mode."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "#+begin_src jupyter-Wolfram-Language" nil t)
      (let ((start (search-forward ":results:" nil t))
            (end   (search-forward ":end:" nil t)))
        (save-restriction
          (narrow-to-region start end)

          ;; Remove ': ' at beginning
          (goto-char (point-min))
          (while (re-search-forward "^: " nil t)
	    (replace-match "" nil nil))

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
            (replace-match "\\1" nil nil))

	  ;; Change 'Out[]' to ': Out[]'
	  (goto-char (point-min))
          (while (re-search-forward "^Out" nil t)
	    (replace-match ": Out" nil nil)))))))

(add-hook 'org-babel-after-execute-hook
          '(lambda ()
             (clean-jupyter-wolfram-language-results)
             (org-latex-preview)
             (org-display-inline-images)))
```

### Executing jupyter-wolfram-language code
First, import the [WolframTerminalImage.wl](https://github.com/TurbulenceChaos/Wolfram-terminal-image/blob/main/WolframTerminalImage.wl) package.

```Mathematica
#+name: Import-Wolfram-terminal-image-package
#+begin_src jupyter-Wolfram-Language :results silent
  (* Get["/path/to/WolframTerminalImage.wl"]; *)
  
  Get["https://raw.githubusercontent.com/TurbulenceChaos/Wolfram-terminal-image/refs/heads/main/WolframTerminalImage.wl"];
  
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

For my emacs configuration, check out: [SCI-emacs](https://github.com/TurbulenceChaos/SCI-emacs).

## Reference

1. [Wolfram Community Discussion](https://community.wolfram.com/groups/-/m/t/2864001)
2. [Mathematica Stack Exchange](https://mathematica.stackexchange.com/questions/258273/how-to-set-up-a-plot-viewer-for-wolfram-engine)
