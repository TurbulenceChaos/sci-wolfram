
Get["/home/peng/.emacs.d/elpa/sci-wolfram/sciWolframDisplayImage.wl"];

(* Display wolfram script image.

Usage:

Default:
$Post = sciWolframDisplayImage[#] &;

All options:
$Post = sciWolframDisplayImage[#,
sciWolframFormulaType	-> "image" (default) or "latex",
sciWolframImageDPI		-> 100 (default),
sciWolframPlay		-> "yes" or "no" (default) to convert plots to CDF interactive file
] &;

Tyep below code to reset $Post:
$Post = .

*)

$Post = sciWolframDisplayImage[#,
sciWolframFormulaType	-> "image",
sciWolframImageDPI	-> 100,
sciWolframPlay		-> "no"
] &;

sol1 = DSolve[{D[y[x, t], t] + 2 D[y[x, t], x] == Sin[x],
y[0, t] == Cos[t]}, y[x, t], {x, t}]

sol2 = sol1[[1, 1, 2]]

Plot3D[sol2, {x, -10, 10}, {t, -5, 5}]
