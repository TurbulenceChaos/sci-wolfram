(* Usage: wolframscript -script /path/to/sci-wolfram-eval.wl /path/to/file.wl *)

(* Import script as a list of deferred expressions *)

file = $ScriptCommandLine[[2]];

exprs = Import[file, "HeldExpressions"];

code =
    Function[expr,
            Module[{i = 0},
                With[{expr = expr /. (HoldComplete :> HoldForm /; i++
                     == 0)},
                    sciWolframDisplay[expr]
                ]
            ]
        ] /@ exprs;

WriteString[$Output, StringRiffle[code, "\n\n"]];
