;;; sci-wolfram-repl.el --- Prettify wolfram symbols -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Peng Peng
;; Created: 2025-05-20
;; Author: Peng Peng <211110103110@stu.just.edu.cn>
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
;; Prettify wolfram symbols
;;
;; Installation:
;;
;; Please check README.md.
;;
;; See https://github.com/TurbulenceChaos/sci-wolfram for more information.

;;; Code:

(defcustom sci-wolfram-prettify-symbols-alist
  '(;; Greek letters
    ("\\[Alpha]" . "α")
    ("\\[Beta]" . "β")
    ("\\[Gamma]" . "γ")
    ("\\[Delta]" . "δ")
    ("\\[Epsilon]" . "ε")
    ("\\[Zeta]" . "ζ")
    ("\\[Eta]" . "η")
    ("\\[Theta]" . "θ")
    ("\\[Iota]" . "ι")
    ("\\[Kappa]" . "κ")
    ("\\[Lambda]" . "λ")
    ("\\[Mu]" . "μ")
    ("\\[Nu]" . "ν")
    ("\\[Xi]" . "ξ")
    ("\\[Omicron]" . "ο")
    ("\\[Pi]" . "π")
    ("\\[Rho]" . "ρ")
    ("\\[Sigma]" . "σ")
    ("\\[Tau]" . "τ")
    ("\\[Upsilon]" . "υ")
    ("\\[Phi]" . "φ")
    ("\\[Chi]" . "χ")
    ("\\[Psi]" . "ψ")
    ("\\[Omega]" . "ω")
    ;; Capital Greek letters
    ("\\[CapitalAlpha]" . "Α")
    ("\\[CapitalBeta]" . "Β")
    ("\\[CapitalGamma]" . "Γ")
    ("\\[CapitalDelta]" . "Δ")
    ("\\[CapitalEpsilon]" . "Ε")
    ("\\[CapitalZeta]" . "Ζ")
    ("\\[CapitalEta]" . "Η")
    ("\\[CapitalTheta]" . "Θ")
    ("\\[CapitalIota]" . "Ι")
    ("\\[CapitalKappa]" . "Κ")
    ("\\[CapitalLambda]" . "Λ")
    ("\\[CapitalMu]" . "Μ")
    ("\\[CapitalNu]" . "Ν")
    ("\\[CapitalXi]" . "Ξ")
    ("\\[CapitalOmicron]" . "Ο")
    ("\\[CapitalPi]" . "Π")
    ("\\[CapitalRho]" . "Ρ")
    ("\\[CapitalSigma]" . "Σ")
    ("\\[CapitalTau]" . "Τ")
    ("\\[CapitalUpsilon]" . "Υ")
    ("\\[CapitalPhi]" . "Φ")
    ("\\[CapitalChi]" . "Χ")
    ("\\[CapitalPsi]" . "Ψ")
    ("\\[CapitalOmega]" . "Ω")
    ;; Mathematical operators
    ("\\[Plus]" . "+")
    ("\\[Minus]" . "−")
    ("\\[Times]" . "×")
    ("\\[Divide]" . "÷")
    ("\\[PlusMinus]" . "±")
    ("\\[MinusPlus]" . "∓")
    ("\\[Sum]" . "∑")
    ("\\[Product]" . "∏")
    ("\\[Integral]" . "∫")
    ("\\[PartialD]" . "∂")
    ("\\[Del]" . "∇")
    ("\\[Square]" . "□")
    ("\\[Diamond]" . "◊")
    ("\\[Circle]" . "○")
    ("\\[CircleTimes]" . "⊗")
    ("\\[CirclePlus]" . "⊕")
    ("\\[CircleDot]" . "⊙")
    ("\\[Cross]" . "⨯")
    ("\\[Star]" . "⋆")
    ("\\[Wedge]" . "∧")
    ("\\[Vee]" . "∨")
    ("\\[And]" . "∧")
    ("\\[Or]" . "∨")
    ("\\[Not]" . "¬")
    ("\\[Implies]" . "⟹")
    ("\\[Equivalent]" . "⇔")
    ("\\[ForAll]" . "∀")
    ("\\[Exists]" . "∃")
    ("\\[NotExists]" . "∄")
    ("\\[Element]" . "∈")
    ("\\[NotElement]" . "∉")
    ("\\[Subset]" . "⊂")
    ("\\[Superset]" . "⊃")
    ("\\[SubsetEqual]" . "⊆")
    ("\\[SupersetEqual]" . "⊇")
    ("\\[Union]" . "∪")
    ("\\[Intersection]" . "∩")
    ("\\[EmptySet]" . "∅")
    ("\\[Infinity]" . "∞")
    ("\\[Degree]" . "°")
    ("\\[Therefore]" . "∴")
    ("\\[Because]" . "∵")
    ("\\[Proportional]" . "∝")
    ("\\[ApproximatelyEqual]" . "≈")
    ("\\[NotEqual]" . "≠")
    ("\\[LessEqual]" . "≤")
    ("\\[GreaterEqual]" . "≥")
    ("\\[MuchLess]" . "≪")
    ("\\[MuchGreater]" . "≫")
    ;; Arrows
    ("\\[Rule]" . "→")
    ("\\[RuleDelayed]" . "⧴")
    ("\\[LeftArrow]" . "←")
    ("\\[RightArrow]" . "→")
    ("\\[UpArrow]" . "↑")
    ("\\[DownArrow]" . "↓")
    ("\\[LeftRightArrow]" . "↔")
    ("\\[UpDownArrow]" . "↕")
    ("\\[DoubleLeftArrow]" . "⇐")
    ("\\[DoubleRightArrow]" . "⇒")
    ("\\[DoubleLeftRightArrow]" . "⇔")
    ("\\[DoubleUpArrow]" . "⇑")
    ("\\[DoubleDownArrow]" . "⇓")
    ("\\[DoubleUpDownArrow]" . "⇕")
    ;; Common mathematical symbols
    ("\\[Sqrt]" . "√")
    ("\\[CubeRoot]" . "∛")
    ("\\[FourthRoot]" . "∜")
    ("\\[Angle]" . "∠")
    ("\\[MeasuredAngle]" . "∡")
    ("\\[SphericalAngle]" . "∢")
    ("\\[Perpendicular]" . "⊥")
    ("\\[Parallel]" . "∥")
    ("\\[NotParallel]" . "∦")
    ;; Special brackets
    ("\\[LeftDoubleBracket]" . "⟦")
    ("\\[RightDoubleBracket]" . "⟧")
    ("\\[LeftAngleBracket]" . "⟨")
    ("\\[RightAngleBracket]" . "⟩")
    ("\\[LeftCeiling]" . "⌈")
    ("\\[RightCeiling]" . "⌉")
    ("\\[LeftFloor]" . "⌊")
    ("\\[RightFloor]" . "⌋"))
  "wolfram prettify symbols alist"
  :type '(alist :key-type string :value-type string)
  :group 'sci-wolfram-mode)

(defun sci-wolfram-prettify-symbols ()
  "Add wolfram prettify symbols alist"
  (setq-local prettify-symbols-alist sci-wolfram-prettify-symbols-alist)
  (setq-local prettify-symbols-compose-predicate (lambda (start end match) t))
  ;; (setq-local prettify-symbols-unprettify-at-point nil)
  (prettify-symbols-mode 1))

(add-hook 'sci-wolfram-mode-hook 'sci-wolfram-prettify-symbols)


(provide 'sci-wolfram-prettify-symbols)
;;; sci-wolfram-prettify-symbols.el ends here
