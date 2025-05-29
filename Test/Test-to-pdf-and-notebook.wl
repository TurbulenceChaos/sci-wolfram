(* ---------------------------------- Title --------------------------------- *)

TextCell["Test.wl", "Title"]

(* --------------------------------- Section -------------------------------- *)

TextCell[Row[{section = 1, " Test formula and plot"}], "Section"]

(* -------------------------------- Paragraph ------------------------------- *)

TextCell["Test formula and plot.", "Text", FontColor -> Gray]

(* ------------------------------- Subsection ------------------------------- *)

TextCell[Row[{section, ".", subsection = 1, " Test formula"}], "Subsection"
    ]

sol1 = DSolve[{D[y[x, t], t] + 2 D[y[x, t], x] == Sin[x], y[0, t] == 
    Cos[t]}, y[x, t], {x, t}]

sol2 = sol1[[1, 1, 2]]

(* ------------------------------- Subsection ------------------------------- *)

TextCell[Row[{section, ".", ++subsection, " Test plot"}], "Subsection"
    ]

Plot3D[sol2, {x, -10, 10}, {t, -5, 5}]

(* --------------------------------- Section -------------------------------- *)

TextCell[Row[{++section, " Test manipulate"}], "Section"]

(* -------------------------------- Paragraph ------------------------------- *)

TextCell["Test manipulate.", "Text", FontColor -> Gray]

Manipulate[Plot[Sin[n x], {x, 0, 2 Pi}], {n, 1, 20}]
