(* Get["https://raw.githubusercontent.com/TurbulenceChaos/Wolfram-terminal-image/refs/heads/main/WolframTerminalImage.wl"]; *)

Get["../sci-wolfram-image.wl"];

(* Code runner of wolfram script: `vscode repl`, `emacs org babel`, or `wolframscript file` *)

sciWolframRunner = "emacs";

(* Image output resolution *)

sciWolframImageDPI = 100;

(* Automatically delete temp images: `yes` (Enable) or `no` (disable) *)

sciWolframDeleteTempFile = "no";

(* Use `wolframplayer` to view `.cdf` files: `yes` (Enable) or `no` (disable) *)

sciWolframPlay = "no";

(* Path to the Wolfram Player

By default, it is set as

sciWolframPlayer =
  FileNameJoin[
    $InstallationDirectory
    ,
    Switch[$OperatingSystem,
      "Unix",
        "Executables"
      ,
      "Windows",
        "Executables"
    ]
    ,
    Switch[$OperatingSystem,
      "Unix",
        "wolframplayer"
      ,
      "Windows",
        "wolframplayer.exe"
    ]
  ];
*)

sciWolframPlayer = "wolframplayer.exe";

(* In repl environment, if you want to restore `$Post` to its original state, just execute `$Post=.` *)

sol1 = DSolve[{D[y[x, t], t] + 2 D[y[x, t], x] == Sin[x], y[0, t] == 
  Cos[t]}, y[x, t], {x, t}]

sol2 = sol1[[1, 1, 2]]

plot = Plot3D[sol2, {x, -10, 10}, {t, -5, 5}]
