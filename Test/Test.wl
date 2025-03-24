Get["WolframTerminalImage`"];

(* Specify the terminal type for Wolfram terminal images (options: "vscode", "emacs") *)

wolframTerminalType = "vscode";

(* Set the resolution (in DPI) for Wolfram terminal images *)

wolframTerminalImageResolution = 100;

(* Enable ("yes") or disable ("no") automatic deletion of Wolfram terminal images *)

wolframTerminalDeleteImage = "no";

(* Enable ("yes") or disable ("no") playback of Wolfram terminal CDF files *)

wolframTerminalPlay = "no";

(* Specify the player application for Wolfram terminal CDF files *)

(* Options: "/path/to/wolframplayer" for Linux or WSL2, "/path/to/wolframplayer.exe" for Windows or WSL2 *)

wolframTerminalPlayer = "wolframplayer";

(* To restore `$Post` to its original state, simply execute "$Post=." *)

sol1 = DSolve[{D[y[x, t], t] + 2 D[y[x, t], x] == Sin[x], y[0, t] == 
  Cos[t]}, y[x, t], {x, t}]

sol2 = sol1[[1, 1, 2]]

Plot3D[sol2, {x, -10, 10}, {t, -5, 5}]
