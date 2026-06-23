(* sciWolframImage.wl

Display wolfram script image.

Usage:

Default:
$Post = sciWolframDisplayImage[#] &;

All options:
$Post = sciWolframDisplayImage[#,
sciWolframFormulaType -> "image" (default) or "latex",
sciWolframImageDPI    -> 100 (default),
sciWolframPlay        -> "yes" or "no" (default) to convert plots to Mahtematica interactive file,
sciWolframShortLines  -> 10 (default): Long expression are displayed using Short[expr, n], where n is the maximum number of lines to show
] &;

Tyep below code to reset $Post:
$Post = .

*)


(*

Get["/path/to/sciWolframDisplayImage.wl"];

$Post = sciWolframDisplayImage[#,
sciWolframFormulaType	-> "image",
sciWolframImageDPI	-> 100,
sciWolframPlay		-> "no",
sciWolframShortLines  -> 10
] &;

*)

sol1 = DSolve[{D[y[x, t], t] + 2 D[y[x, t], x] == Sin[x], y[0, t] == Cos[t]}, y[x, t], {x, t}]

sol2 = sol1[[1, 1, 2]]

Plot3D[sol2, {x, -10, 10}, {t, -5, 5}]

Manipulate[Plot[Sin[n x], {x, 0, 2 Pi}], {n, 1, 20}]

MatrixForm[Array[Subscript[a, ##]&, {2, 2, 2}]]
