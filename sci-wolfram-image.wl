(* ::Package:: *)

BeginPackage["sciWolframImage`"];

sciWolframRunner::usage = "Code runner of wolfram script: `vscode`, `emacs`"

sciWolframImageDPI::usage = "Image output resolution"

sciWolframDeleteTempFile::usage = "Automatically delete temp images: `yes` (Enable) or `no` (disable)"

sciWolframFormulaType::usage = "Formula output type: `latex` or `image`"

sciWolframPlay::usage = "Use `wolframplayer` to view `.cdf` files: `yes` (Enable) or `no` (disable)"

sciWolframPlayer::usage = "Path to the Wolfram Player"

sciWolframDisplay::usage = "Display outputs as different formats"

Begin["`Private`"];

(* `%` operator works only in repl *)

sciWolframRunner = "vscode";

sciWolframImageDPI = 100;

sciWolframDeleteTempFile = "no";

sciWolframFormulaType = "latex";

sciWolframPlay = "no";

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

(* Initialize output counter *)

n = 1;

sciWolframEnvironmentType[] :=
  If[MatchQ[$ScriptCommandLine, {_String, ___}],
    "script"
    ,
    "repl"
  ]

(* Function to display plain text output *)

sciWolframText[expr_] :=
  Module[{},
    Print[StringForm["Out[`1`]=", n++]];
    Print[expr];
    (* Return the original expr *)
    If[sciWolframEnvironmentType[] == "script",
      expr
      ,
      expr;
    ]
  ]

(* Function to display LaTeX output *)

sciWolframTeX[expr_] :=
  Module[{},
    Print[StringForm["Out[`1`]=", n++]];
    Print["\\begin{equation*}\n" <> ToString[TeXForm[expr]] <> "\n\\end{equation*}"
      ];
    (* Return the original expr *)
    If[sciWolframEnvironmentType[] == "script",
      expr
      ,
      expr;
    ]
  ];

sciWolframImage[expr_, playCDF_] :=
  Module[{dir, filePNG, fileCDF},
    dir = FileNameJoin[{Directory[], "tmp", "wolfram"}];
    filePNG = FileNameJoin[{dir, CreateUUID["wolfram-"] <> ".png"}];
    fileCDF = StringReplace[filePNG, ".png" -> ".cdf"];
    If[$VersionNumber < 12.2,
      If[!DirectoryQ[dir],
        CreateDirectory[dir, CreateIntermediateDirectories -> True]
      ]
    ];
    (* Export expr as image *)
    Export[filePNG, Notebook[{Cell @ BoxData @ ToBoxes @ expr}], ImageResolution
       -> sciWolframImageDPI];
    (* Display image *)
    Switch[sciWolframRunner,
      "emacs",
        Print[StringForm["Out[`1`]=", n++]];
        Print["[[file:" <> FileNameDrop[filePNG, FileNameDepth @ Directory[
          ]] <> "]]"]
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
      Export[fileCDF, Notebook[{Cell @ BoxData @ ToBoxes @ expr}]];
      StartProcess[{sciWolframPlayer, FileNameTake[fileCDF]}, ProcessDirectory
         -> DirectoryName[fileCDF]]
    ];
    (* Return the original expr *)
    If[sciWolframEnvironmentType[] == "script",
      expr
      ,
      expr;
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
        Switch[sciWolframRunner,
          "emacs",
            If[MatchQ[expr, Null],
              expr
              ,
              sciWolframText[expr]
            ]
          ,
          "vscode",
            expr
        ]
      ,
      True,
        Switch[sciWolframRunner,
          "emacs",
            Switch[sciWolframFormulaType,
              "latex",
                If[isPlot,
                  sciWolframImage[expr, playCDF = sciWolframPlay]
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
