(* title *)

TextCell["Script To Notebook.wl", "Title"]

(* section *)

TextCell[StringTemplate["`1` Test Formula and Plot"][section = 1], "Section"]

(* paragraph *)

TextCell["Test formula and plot.", "Text", FontColor -> Gray]

(* subsection *)

TextCell[StringTemplate["`1`.`2` Test Formula"][section, subsection = 1], "Subsection"]

sol1 = DSolve[{D[y[x, t], t] + 2 D[y[x, t], x] == Sin[x], y[0, t] == Cos[t]}, y[x, t], {x, t}]

sol2 = sol1[[1, 1, 2]]

(* subsection *)

TextCell[StringTemplate["`1`.`2` Test Plot"][section, ++subsection], "Subsection"]

Plot3D[sol2, {x, -10, 10}, {t, -5, 5}]

(* section *)

TextCell[StringTemplate["`1` Test Manipulate"][++section], "Section"]

(* paragraph *)

TextCell["Test Manipulate.", "Text", FontColor -> Gray]

Manipulate[Plot[Sin[n x], {x, 0, 2 Pi}], {n, 1, 20}]
