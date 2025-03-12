Get["WolframTerminalImage`"];

(* Enable or disable auto-deletion of Wolfram terminal images *)

wolframTerminalDeleteImage = "no";

(* Resolution of Wolfram terminal images *)

wolframTerminalImageResolution = 100;

sol1 = DSolve[{D[y[x, t], t] + 2 D[y[x, t], x] == Sin[x], y[0, t] == 
  Cos[t]}, y[x, t], {x, t}]

sol2 = sol1[[1, 1, 2]]

Plot3D[sol2, {x, -10, 10}, {t, -5, 5}]
