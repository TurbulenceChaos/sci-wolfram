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
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; 1. Install emacs-jupyter
;; https://github.com/emacs-jupyter/jupyter
(unless (package-installed-p 'jupyter)
  (package-install 'jupyter))

;; For emacs 29+, you can use `package-vc-install` to install packages directly from github;
;; otherwise you can manually download the package and add it to `load-path`
;; (add-to-list 'load-path "/path/to/package/")

;; 2. Install wolfram-mode for syntax highlight, code format and completion
;; https://github.com/xahlee/xah-wolfram-mode
(unless (package-installed-p 'xah-wolfram-mode)
  (package-vc-install "https://github.com/xahlee/xah-wolfram-mode.git"))
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

;; 4. Install wolfram-terminal-image for processing jupyter-wolfram-language results
(unless (package-installed-p 'wolfram-terminal-image)
  (package-vc-install "https://github.com/TurbulenceChaos/Wolfram-terminal-image.git"))

;; t (default) for converting wolfram formula to latex;
;; otherwise nil for converting wolfram formula to image
(setq wolfram-terminal-formula-type=latex t)

;; minimum number of lines for block output
(if wolfram-terminal-formula-type=latex
    (setq org-babel-min-lines-for-block-output 100)
  (setq org-babel-min-lines-for-block-output 20))
```

### Executing jupyter-Wolfram-Language code in org-mode
First, import the [WolframTerminalImage.wl](https://github.com/TurbulenceChaos/Wolfram-terminal-image/blob/main/WolframTerminalImage.wl) package.

```Mathematica
#+name: Import-Wolfram-terminal-image-package
#+begin_src jupyter-Wolfram-Language :results silent
  (* Get["https://raw.githubusercontent.com/TurbulenceChaos/Wolfram-terminal-image/refs/heads/main/WolframTerminalImage.wl"]; *)
  
  Get["~/.emacs.d/elpa/Wolfram-terminal-image/WolframTerminalImage.wl"];
  
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
