(* Get["/path/to/sci-wolfram-image.wl"]; *)

(* 1. sciWolframEnv

Environment: "vscode" (where inline images are displayed by `imgcat`), "emacs" (where images are printed as file link)

By default, it was set as If[Environment["TERM_PROGRAM"] == "vscode", "vscode", "emacs"]

You can also specify it by

sciWolframEnv = "vscode";

*)

(* 2. sciWolframImageDPI

Image output resolution: Image output resolution: 100 DPI in "vscode" and  150 DPI in "emacs" by default

You can specify it by

sciWolframImageDPI = 150;

*)

(* 3. sciWolframFormulaType

For emacs, formula output types: "latex" or "image" (by default)

You can specify it by

sciWolframFormulaType = "latex"; 

*)

(* 4. sciWolframOrigExpr

Whether to display both inline images and original expression: "yes" (Enable) or "no" (disable by default)

You can specify it by

sciWolframOrigExpr = "yes";

*)

(* 5. sciWolframPlay

Whether to use `wolframplayer` to view `.cdf` files: "yes" (Enable) or "no" (disable by default)

You can specify it by

sciWolframPlay = "yes";

*)

(* 6. sciWolframPlayer

Path to the Wolfram Player, which is set as FileNames["*wolframplayer*", $InstallationDirectory, 2][[1]] by default

You can also specify the path by

sciWolframPlayer = "/path/to/wolframplayer"; (for linux)

sciWolframPlayer = "/path/to/wolframplayer.exe"; (for windows)

*)

(* 7. sciWolframDisplay

Display outputs as different formats

$Post does not work in command line, 

so you have to explicitly apply `sciWolframDisplay` function to the expression that you want to display as image

For command line eval:

wolframscript -script example.wl

where example.wl:

Get["/path/to/sci-wolfram-image.wl"];

sol1 = DSolve[{D[y[x, t], t] + 2 D[y[x, t], x] == Sin[x], y[0, t] == 
    Cos[t]}, y[x, t], {x, t}] // sciWolframDisplay

sol2 = sol1[[1, 1, 2]];

plot = Plot3D[sol2, {x, -10, 10}, {t, -5, 5}] // sciWolframDisplay 

*)

(* 8. Notes: 

1. In repl environment,

if you want to restore `$Post` to its original state, just execute

$Post=.

2. `%` operator works only in repl env

*)

sol1 = DSolve[{D[y[x, t], t] + 2 D[y[x, t], x] == Sin[x], y[0, t] == 
    Cos[t]}, y[x, t], {x, t}]

sol2 = sol1[[1, 1, 2]]

plot = Plot3D[sol2, {x, -10, 10}, {t, -5, 5}]
