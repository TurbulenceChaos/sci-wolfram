(* DisplayImage.wl

Display Wolfram script images.

Usage:

Default:
$Post = DisplayImage[#] &;

All options:
$Post = DisplayImage[#,
ExprType   -> "image" (default) or "latex",
ImageDPI   -> 150 (default),
PlotPlay   -> "yes" or "no" (default) to convert dynamic plots to Wolfram Mathematica interactive files,
ShortLines -> 10 (default): use Short[code, n] to print long expressions less than n lines
] &;

Tyep below code to reset $Post:
$Post = .

*)

(*

Get["/path/to/DisplayImage.wl"];

$Post = DisplayImage[#,
ExprType   -> "image",
ImageDPI   -> 150,
PlotPlay   -> "no",
ShortLines -> 10
] &;

*)

sol1 = DSolve[{D[y[x, t], t] + 2 D[y[x, t], x] == Sin[x], y[0, t] == Cos[t]}, y[x, t], {x, t}]

sol2 = sol1[[1, 1, 2]]

Plot3D[sol2, {x, -10, 10}, {t, -5, 5}]

Manipulate[Plot[Sin[n x], {x, 0, 2 Pi}], {n, 1, 20}]

MatrixForm[Array[Subscript[a, ##]&, {2, 2, 2}]]
