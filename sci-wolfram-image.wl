(* ::Package:: *)

BeginPackage["sciWolframImage`"];

sciWolframEnv::usage = "sciWolframEnv

Environment: \"vscode\" (where inline images are displayed by `imgcat`), \"emacs\" (where images are printed as file link)

By default, it was set as If[Environment[\"TERM_PROGRAM\"] == \"vscode\", \"vscode\", \"emacs\"]

You can also specify it by

sciWolframEnv = \"vscode\";
"

sciWolframImageDPI::usage = "sciWolframImageDPI

Image output resolution: 100 DPI in \"vscode\" and  150 DPI in \"emacs\" by default

You can specify it by

sciWolframImageDPI = 150;
"

sciWolframDeleteTempFile::usage = "sciWolframDeleteTempFile

Automatically delete temp images: \"yes\" (Enable) or \"no\" (disable by default)
"

sciWolframFormulaType::usage = "sciWolframFormulaType

For emacs, formula output types: \"latex\" or \"image\" (by default)

You can specify it by

sciWolframFormulaType = \"latex\";
"

sciWolframOrigExpr::usage = "sciWolframOrigExpr

Whether to display both inline images and original expression: \"yes\" (Enable) or \"no\" (disable by default)

You can specify it by

sciWolframOrigExpr = \"yes\"; 
"

sciWolframPlay::usage = "sciWolframPlay

Whether to use `wolframplayer` to view `.cdf` files: \"yes\" (Enable) or \"no\" (disable by default)

You can specify it by

sciWolframPlay = \"yes\";
"

sciWolframPlayer::usage = "sciWolframPlayer

Path to the Wolfram Player, which is set as FileNames[\"*wolframplayer*\", $InstallationDirectory, 2][[1]] by default

You can also specify the path by

sciWolframPlayer = \"/path/to/wolframplayer\"; (for linux)

sciWolframPlayer = \"/path/to/wolframplayer.exe\"; (for windows)
"

sciWolframDisplay::usage = "sciWolframDisplay

Display outputs as different formats

$Post does not work in command line, 

so you have to explicitly apply `sciWolframDisplay` function to the expression that you want to display as image

For command line eval:

wolframscript -script example.wl

where example.wl:

Get[\"/path/to/sci-wolfram-image.wl\"];

sol1 = DSolve[{D[y[x, t], t] + 2 D[y[x, t], x] == Sin[x], y[0, t] == 
    Cos[t]}, y[x, t], {x, t}] // sciWolframDisplay

sol2 = sol1[[1, 1, 2]];

plot = Plot3D[sol2, {x, -10, 10}, {t, -5, 5}] // sciWolframDisplay
"

(* Notes: 

1. In repl environment,

if you want to restore `$Post` to its original state, just execute

$Post=.

2. `%` operator works only in repl env

*)

Begin["`Private`"];

sciWolframEnv =
    If[Environment["TERM_PROGRAM"] === "vscode",
        "vscode"
        ,
        "emacs"
    ];

sciWolframImageDPI =
    Switch[sciWolframEnv,
        "vscode",
            100
        ,
        "emacs",
            150
    ];

sciWolframDeleteTempFile = "no";

sciWolframFormulaType = "image";

sciWolframPlay = "no";

sciWolframPlayer = FileNames["*wolframplayer*", $InstallationDirectory,
     2][[1]];

sciWolframOrigExpr = "no";

(* Initialize output counter *)

n = 1;

(* check if it is in repl or script *)

sciWolframRunner[] :=
    If[MatchQ[$ScriptCommandLine, {_String, ___}],
        "script"
        ,
        "repl"
    ];

(* Function to display plain text output *)

sciWolframText[expr_] :=
    Module[{},
        Print[ToString[StringForm[": Out[`1`]= ", n++]]];
        Print[expr];
        (* Return the original expr *)
        If[sciWolframOrigExpr == "yes",
            Print[expr];
            expr;
            ,
            If[sciWolframRunner[] == "script",
                expr
                ,
                expr;
            ]
        ]
    ];

(* Function to display LaTeX output *)

sciWolframTeX[expr_] :=
    Module[{},
        Print[ToString[StringForm[": Out[`1`]= ", n++]]];
        Print["\\begin{equation*}\n" <> ToString[TeXForm[expr]] <> "\n\\end{equation*}"
            ];
        (* Return the original expr *)
        If[sciWolframOrigExpr == "yes",
            Print[expr];
            expr;
            ,
            If[sciWolframRunner[] == "script",
                expr
                ,
                expr;
            ]
        ]
    ];

sciWolframImage[expr_, playCDF_] :=
    Module[{dir, filePNG, fileCDF},
        dir = FileNameJoin[{Directory[], "tmp", "wolfram"}];
        filePNG = FileNameJoin[{dir, CreateUUID["wolfram-"] <> ".png"
            }];
        fileCDF = StringReplace[filePNG, ".png" -> ".cdf"];
        If[$VersionNumber < 12.2,
            If[!DirectoryQ[dir],
                CreateDirectory[dir, CreateIntermediateDirectories ->
                     True]
            ]
        ];
        (* Export expr as image *)
        Export[filePNG, Notebook[{Cell @ BoxData @ ToBoxes @ expr}], 
            ImageResolution -> sciWolframImageDPI];
        (* Display image *)
        Switch[sciWolframEnv,
            "emacs",
                Print[ToString[StringForm[": Out[`1`]= ", n++]]];
                Print["[[file:" <> FileNameDrop[filePNG, FileNameDepth
                     @ Directory[]] <> "]]"]
            ,
            "vscode",
                Run["imgcat " <> filePNG];
                (* Delete temp file *)
                If[sciWolframDeleteTempFile == "yes",
                    Quiet @ DeleteFile @ filePNG
                ];
        ];
        (* Use `wolframplayer` to view `.cdf` files *)
        If[playCDF == "yes",
            Export[fileCDF, Notebook[{Cell @ BoxData @ ToBoxes @ expr
                }]];
            StartProcess[{sciWolframPlayer, FileNameTake[fileCDF]}, ProcessDirectory
                 -> DirectoryName[fileCDF]]
        ];
        (* Return the original expr *)
        If[sciWolframOrigExpr == "yes",
            Print[expr];
            expr;
            ,
            If[sciWolframRunner[] == "script",
                expr
                ,
                expr;
            ]
        ]
    ];

sciWolframDisplay[expr_] :=
    Module[{box, isFormula, isPlot},
        box = ToBoxes[expr];
        isFormula = !FreeQ[box, RowBox | SqrtBox | SuperscriptBox];
        isPlot = !FreeQ[box, DynamicBox | DynamicModuleBox | GraphicsBox
             | Graphics3DBox];
        Which[
            !isFormula && !isPlot,
                Switch[sciWolframEnv,
                    "emacs",
                        If[MatchQ[expr, Null],
                            expr
                            ,
                            sciWolframText[expr]
                        ]
                    ,
                    "vscode",
                        Print[expr]
                ]
            ,
            True,
                Switch[sciWolframEnv,
                    "emacs",
                        Switch[sciWolframFormulaType,
                            "latex",
                                If[isPlot,
                                    sciWolframImage[expr, playCDF = sciWolframPlay
                                        ]
                                    ,
                                    sciWolframTeX[expr]
                                ]
                            ,
                            "image",
                                sciWolframImage[
                                    expr
                                    ,
                                    playCDF =
                                        If[isPlot,
                                            sciWolframPlay
                                            ,
                                            "no"
                                        ]
                                ]
                        ]
                    ,
                    "vscode",
                        sciWolframImage[
                            expr
                            ,
                            playCDF =
                                If[isPlot,
                                    sciWolframPlay
                                    ,
                                    "no"
                                ]
                        ]
                ]
        ]
    ];

End[];

EndPackage[];

$Post = sciWolframDisplay[#]&;
