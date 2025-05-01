(* ::Package:: *)

BeginPackage["WolframTerminalImage`"];

(* Specify the terminal type for Wolfram terminal images (options: "vscode", "emacs") *)

wolframTerminalType = "vscode";

(* Set the resolution (in DPI) for Wolfram terminal images *)

wolframTerminalImageResolution = 100;

(* Enable ("yes") or disable ("no") automatic deletion of Wolfram terminal images for vscode *)

wolframTerminalDeleteImage = "no";

(* Specify the formula type for emacs (options: "latex", "image") *)

wolframTerminalFormulaType = "latex";

(* Enable ("yes") or disable ("no") playback of Wolfram terminal CDF files *)

wolframTerminalPlay = "no";

(* Specify the player application for Wolfram terminal CDF files *)

(* Options: "/path/to/wolframplayer" for Linux or WSL2, "/path/to/wolframplayer.exe" for Windows or WSL2 *)

wolframTerminalPlayer = "wolframplayer";

Begin["`Private`"];

(* TODO: The '%' operator works only in terminal mode *)
(* TODO: Use $PrePrint to print output counter *)

(* Initialize output counter *)
n = 1;

(* Function to display plain text output *)
WolframTerminalText[expr_] :=
  Module[{},
    (* Print the output number counter *)
    Print[StringForm["Out[`1`]=", n++]];
    
    (* Print the expression itself *)
    Print[expr];

    (* Return the original expression *)
    expr;
  ];

(* Function to display LaTeX output *)
WolframTerminalTeX[expr_] :=
  Module[{},
    (* Print the output number counter *)
    Print[StringForm["Out[`1`]=", n++]];
    
    (* Convert expression to LaTeX and wrap in equation environment *)  
    Print["\\begin{equation*}\n" <> ToString[TeXForm[expr]] <> "\n\\end{equation*}"];
    
    (* Return the original expression *)
    expr;
  ];

WolframTerminalImage[expr_, playCDF_] :=
  Module[
    {dir, filePNG, fileCDF},
    (* Set up directory and file paths *)
    dir = FileNameJoin[{Directory[], "tmp", "wolfram"}];
    filePNG = FileNameJoin[{dir, CreateUUID["wolfram-"] <> ".png"}];
    fileCDF = StringReplace[filePNG, ".png" -> ".cdf"];
    
    (* Create directory if needed (for older Wolfram versions) *)
    If[$VersionNumber < 12.2,
      If[!DirectoryQ[dir], CreateDirectory[dir, CreateIntermediateDirectories -> True]]
    ];
    
    (* Export expression as PNG *)
    Export[filePNG, Notebook[{Cell @ BoxData @ ToBoxes @ expr}], ImageResolution -> wolframTerminalImageResolution];
    
    (* Handle display based on terminal type *)
    Switch[wolframTerminalType,
      "emacs",
        Print[StringForm["Out[`1`]=", n++]];
        Print["[[file:" <> FileNameDrop[filePNG, FileNameDepth @ Directory[]] <> "]]"],
      "vscode",
        Run["imgcat " <> filePNG];
        If[wolframTerminalDeleteImage == "yes", Quiet @ DeleteFile @ filePNG]
    ];
    
    (* Handle CDF export and playback if requested *)
    If[playCDF == "yes",
      Export[fileCDF, Notebook[{Cell @ BoxData @ ToBoxes @ expr}]];
      StartProcess[{wolframTerminalPlayer, FileNameTake[fileCDF]}, ProcessDirectory -> DirectoryName[fileCDF]]
    ];
    
    (* Return the original expression *)
    expr;
  ];

$Post =
  Module[
    {box, isFormula, isPlot},
    (* Convert input to box representation *)
    box = ToBoxes[#];
    (* Check content type *)
    isFormula = !FreeQ[box, RowBox | SqrtBox | SuperscriptBox];
    isPlot = !FreeQ[box, DynamicBox | DynamicModuleBox | GraphicsBox | Graphics3DBox];
    
    (* Handle different output formats based on content type and terminal *)
    Which[
      (* Plain text - neither formula nor plot *)
      !isFormula && !isPlot,
        Switch[wolframTerminalType,
          "emacs", If[MatchQ[#, Null], #, WolframTerminalText[#]],
          "vscode", #
        ],
      
      (* Formula or plot content *)
      True,
        Switch[wolframTerminalType,
          "emacs",
            Switch[wolframTerminalFormulaType,
              "latex",
                If[isPlot,
                  WolframTerminalImage[#, playCDF = wolframTerminalPlay],
                  WolframTerminalTeX[#]
                ],
              "image",
                WolframTerminalImage[#, playCDF = If[isPlot, wolframTerminalPlay, "no"]]
            ],
          "vscode",
            WolframTerminalImage[#, playCDF = If[isPlot, wolframTerminalPlay, "no"]]
        ]
    ]
  ]&;

End[];

EndPackage[];
