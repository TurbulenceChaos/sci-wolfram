# sci-wolfram
Author: Peng Peng

Email: [211110103110@stu.just.edu.cn](mailto:211110103110@stu.just.edu.cn)

GitHub: [TurbulenceChaos/sci-wolfram](https://github.com/TurbulenceChaos/sci-wolfram)

# Table of Contents
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
- [Introduction](#introduction)
- [Features for Emacs](#features-for-emacs)
- [Features for VSCode](#features-for-vscode)
- [Installation for Emacs](#installation-for-emacs)
  - [Prerequisites](#prerequisites)
  - [Configuration](#configuration)
- [Usage for Emacs](#usage-for-emacs)
  - [For Wolfram Script File](#for-wolfram-script-file)
  - [For Wolfram Src-block in Org-mode](#for-wolfram-src-block-in-org-mode)
- [Installation for VSCode](#installation-for-vscode)
  - [Prerequisites](#prerequisites-1)
  - [Configuration](#configuration-1)
- [Usage for VSCode](#usage-for-vscode)
  - [For Wolfram REPL](#for-wolfram-repl)
  - [Convert Wolfram Script to PDF and Mathematica Notebook](#convert-wolfram-script-to-pdf-and-mathematica-notebook)
- [Change Log](#change-log)
  - [v3.0.0](#v300)
- [Reference](#reference)

<!-- markdown-toc end -->

# Introduction
An all-in-one Wolfram Mathematica package for Emacs.

Display wolfram script images in Visual Studio Code Terminal.

# Features for Emacs
- [x] **Display wolfram script as images, LaTeX, and Wolfram Player interactive files**
- [x] **Support async Wolfram REPL session calc**
- [x] Automatic completion and formatting with `LSPServer`
- [x] Convert wolfram script to PDF and Mathematica notebook
- [x] Prettify Mathematica symbols

# Features for VSCode
- [x] **Display wolfram script images in VSCode Terminal using `imgcat`**
- [x] Convert wolfram script to PDF and Mathematica notebook

# Installation for Emacs
## Prerequisites
- **Wolfram Engine** (**free**, already include **wolframscript** and **Wolfram Player**), or **Mathematica**\
Required for running wolfram scripts, which can be downloaded from https://www.wolfram.com/download-center/index.php.en
- [LaTeX](https://orgmode.org/manual/Previewing-LaTeX-fragments.html) (optional)\
Preview latex formula in emacs org-mode:
  - Linux: `sudo apt install texlive-full`
  - Windows: install [MiKTeX](https://miktex.org/howto/install-miktex)

## Configuration
```lisp
;; For emacs 29+, you can use `package-vc-install' to install packages from github
(unless (package-installed-p 'sci-wolfram)
  (package-vc-install "https://github.com/TurbulenceChaos/sci-wolfram"))

;; (add-hook 'sci-wolfram-mode-hook 'eglot-ensure) ; eglot
;; (add-hook 'sci-wolfram-mode-hook 'lsp-deferred) ; or lsp-mode

;; or you can manually download the package and add it to `load-path'
;; (add-to-list 'load-path "/path/to/sci-wolfram")
;; (require 'sci-wolfram)

;; to customize all configurable variables of `sci-wolfram' package,
;; just type M-x customize-group RET sci-wolfram-mode RET
```

For [Doom Emacs](https://github.com/doomemacs/doomemacs) users (see [this discussion](https://github.com/TurbulenceChaos/sci-wolfram/issues/4)):

```elisp
;; doom/packages.el
(package! sci-wolfram
  :recipe (:host github
           :repo "TurbulenceChaos/sci-wolfram"
           :files ("*.el" "*.wl")))
```

```elisp
;; doom/config.el
(use-package! sci-wolfram
  :defer t
  :config
  ;; (add-hook 'sci-wolfram-mode-hook 'eglot-ensure) ; eglot
  ;; (add-hook 'sci-wolfram-mode-hook 'lsp-deferred) ; or lsp-mode
  )

;; to customize all configurable variables of `sci-wolfram' package,
;; just type M-x customize-group RET sci-wolfram-mode RET
```

# Usage for Emacs
Default leader key in `sci-wolfram-mode` is `C-c`.

Or You can just type `M-x sci-wolfram-` to use all interactive functions.

## For Wolfram Script File
`sci-wolfram-run-region-or-buffer`

Display images, latex fragments, and Wolfram Player interactive files.

Support async Wolfram REPL session calc.



`eglot` or `lsp-mode`

Support auto-completion and formatting.



`sci-wolfram-convert-to-notebook`

Convert region or buffer code to PDF and Mathematica notebook.

You can using `TextCell` to add title, subtitle, section, text, formula, etc to Mathematica notebook.



`sci-wolfram-prettify-symbols`

Toggle with `M-x prettify-symbols-mode`



`sci-wolfram-doc-lookup`

Search wolfram doc in web.



## For Wolfram Src-block in Org-mode

`org-babel-execute:wolfram`

Display images, latex fragments, and Wolfram Player interactive files.

**Support async session src-block calc.**



`completion-at-point`

Support auto-completion inside wolfram src-block.


`sci-wolfram-format-region-or-buffer`

Format region or wolfram src-block code.



`sci-wolfram-convert-to-notebook`

Convert region or wolfram src-block code to PDF and Mathematica notebook.

You can using `TextCell` to add title, subtitle, section, text, formula, etc to Mathematica notebook.



> [!IMPORTANT]
> 1. `Print[code]` will truncate string lines in Wolfram REPL. Consider using `WriteString["stdout", code, "\n"]` instead.
> 2. For emacs, Wolfram REPL use `wolframscript -rawterm`, where space lines in string are not allowed:\
```Mathematica
str="First line

Second line"
```
> All space lines are automatically removed from strings before running codes.
> 3. For org-mode version < 9.8, spaces before a string (i.e. "   Hello") in src-block session async running will be automatically removed.\
> For org-mode version >= 9.8, `org-babel-comint-async-register` function introduced a new option `disable-prompt-filtering` to avoid this situation.

> Please read [Test.org](Test/Test.org) for more details.

# Installation for VSCode
## Prerequisites

- **Wolfram Engine** (**free**, already include **wolframscript** and **Wolfram Player**), or **Mathematica**\
Required for running wolfram scripts, which can be downloaded from https://www.wolfram.com/download-center/index.php.en
- [`imgcat`](https://github.com/wookayin/python-imgcat)\
Enable image display in VSCode Terminal.\
Install with `pip install imgcat`

## Configuration

**Make sure your system is using a discrete graphics card!**

- Enable `Terminal > Integrated: Enable Images` and `Terminal > Integrated: GPU Acceleration` in VSCode settings, and 

![vscode-official-wolfram-extension.png](Images/vscode-terminal-enable-display-images.png "title")

- Install the official [wolfram language extension](https://github.com/WolframResearch/vscode-wolfram) from vscode extension marketplace.

![vscode-official-wolfram-extension.png](Images/vscode-official-wolfram-extension.png)

# Usage for VSCode
## For Wolfram REPL
Steps:
1. `Ctrl+Shift+P`: `Wolfram Language: Start Wolfram in Terminal`
2. Modify and paste below code into REPL:
```Mathematica
Get["/path/to/sciWolframDisplayImage.wl"];

$Post = sciWolframDisplayImage[#] &;
```
3. Select codes in wolfram script file and send them into REPL by `Terminal: Run Selected Text In Active Terminal`

See [TestDisplayImages.wl](Test/TestDisplayImages.wl) for more details about configurable parameters of `sciWolframDisplayImage` package.



## Convert Wolfram Script to PDF and Mathematica Notebook
You can using `TextCell` to add title, subtitle, section, text, formula, etc to Mathematica notebook.

Steps:
1. `Ctrl+Shift+P`: `Wolfram Language: Start Wolfram in Terminal`
2. Modify and paste below code into REPL:
```Mathematica
Get["/path/to/sciWolframConvertToNotebook.wl"];

sciWolframConvertToNotebook["/path/to/file.wl"];
```
See [TestConvertToNotebook.wl](Test/TestConvertToNotebook.wl) and [file.wl](Test/file.wl) for details.

# Change Log
## v3.0.0
- **Add Wolfram REPL async session calc support**
- **Remove `emacs-jupyter` dependency**
- Refactor `sciWolframDisplayImage.wl` and `sciWolframConvertToNotebook.wl` package

# Reference
For `sciWolframDisplayImage.wl`:\
Thanks to the following two discussions for guidance on displaying wolfram images and interactive CDF files:
- [Displaying graphics and images inline on Wolfram Engine from Wolfram Community Discussion](https://community.wolfram.com/groups/-/m/t/2864001)
- [How to set up a Plot viewer for Wolfram Engine from Mathematica Stack Exchange](https://mathematica.stackexchange.com/questions/258273/how-to-set-up-a-plot-viewer-for-wolfram-engine)

For `sci-wolfram-mode`:\
Thanks to `xah-wolfram-mode` by Xah Lee, `wolfram-mode` by Kawabata, and `wolfram-mode` by Daniel Nicolai for their inspiration and valuable references.
- [xah-wolfram-mode](https://github.com/xahlee/xah-wolfram-mode)
- [wolfram-mode](https://github.com/kawabata/wolfram-mode)
- [wolfram-mode](https://github.com/dalanicolai/wolfram-mode/tree/master)

For `LSPServer`:\
Thanks to `wolfram-language-mode` by Oliver Grasl and `vscode-wolfram` by Wolfram Research for valuable references on `LSPServer`:
- [wolfram-language-mode](https://github.com/transentis/wolfram-language-modse)
- [vscode-wolfram](https://github.com/WolframResearch/vscode-wolfram)

For `sciWolframConvertToNotebook.wl`:\
Thanks to the following two discussions for guidance on converting wolfram scripts to PDF files:
- [Converting Wolfram Language Scripts into PDFs from Mathematica Stack Exchange](https://mathematica.stackexchange.com/questions/293543/converting-wolfram-language-scripts-wls-into-pdfs)
- [How to get Defer to behave with Manipulate/Dynamic from Wolfram Community](https://community.wolfram.com/groups/-/m/t/37054)

