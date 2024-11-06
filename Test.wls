(* "WolframDisplay[display, img] sets $Post to present formulas and plots either as images or text, based on the provided display and img parameters.

Parameters:
`display`: Specifies how formulas and plots should be presented. 
           Options include "Text" (text only for both formulas and plots), "InlinePNG" (using imgcat), "ExternalPNG" (opening images with IrfanView), and "WolframCDF" (converting expressions to CDF files and opening them with WolframPlayer);
`img`: Defines the type of content to display. 
       Options are "Formula" (applicable to formulas), "Plot" (applicable to plots), and "FormulaAndPlot" (applies to both formulas and plots). *)

(* The default configuration is WolframDisplay["InlinePNG", "FormulaAndPlot"], which does not need to be explicitly stated. Other image display methods include WolframDisplay["WolframCDF", "FormulaAndPlot"], WolframDisplay["ExternalPNG", "FormulaAndPlot"], etc. *)

(* For individual expressions, you can use "// WolframCDF" or "// WolframPNG". *)

Get["WolframDisplay`"];

WolframDisplay["InlinePNG", "FormulaAndPlot"];

sol1 = DSolve[{D[y[x, t], t] + 2 D[y[x, t], x] == Sin[x], y[0, t] == 
  Cos[t]}, y[x, t], {x, t}] // WolframCDF

sol2 = sol1[[1, 1, 2]]

Plot3D[sol2, {x, -10, 10}, {t, -5, 5}] // WolframCDF

Manipulate[Plot3D[Sin[x + a] Cos[y], {x, 0, 2 Pi}, {y, 0, 2 Pi}], {a,
   0, 2 Pi}] // WolframPNG
