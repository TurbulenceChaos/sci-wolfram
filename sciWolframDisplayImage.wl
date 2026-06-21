(* Display wolfram script image *)

BeginPackage["sciWolframDisplayImage`"];

sciWolframDisplayImage::usage = "Display wolfram script image.
Usage:
Default:
$Post = sciWolframDisplayImage[#] &;
All options:
$Post = sciWolframDisplayImage[#,
sciWolframFormulaType	-> \"image\" (default) or \"latex\",
sciWolframImageDPI	-> 100 (default),
sciWolframPlay		-> \"yes\" or \"no\" (default) to convert plots to Mathematica interactive file
] &;
Tyep below code to reset $Post:
$Post = .
";

Begin["`Private`"];

(* Initialize output counter *)

n = 1;

(* Display plain text output *)

sciWolframText[expr_] :=
	Module[{},
		WriteString["stdout", StringTemplate[": Out[`1`]= "][n++], "\n"];
		WriteString["stdout", expr, "\n"];
	];

(* Display LaTeX output *)

sciWolframTeX[expr_] :=
	Module[{},
		WriteString["stdout", StringTemplate[": Out[`1`]= "][n++], "\n"];
		WriteString["stdout", "\\begin{equation*}", "\n"];
		WriteString["stdout", TeXForm[expr], "\n"];
		WriteString["stdout", "\\end{equation*}", "\n"];
	];

sciWolframEnv =
	If[SameQ[Environment["TERM_PROGRAM"], "vscode"],
		"vscode"
		,
		"emacs"
	];

sciWolframPlayer = First[FileNames[{"*wolframplayer*", "*WolframNB*"}, $InstallationDirectory, 2], Null];

(* Display Image output *)

sciWolframImage[expr_, sciWolframImageDPI_, playNB_] :=
	Module[{dir, sciWolframImageDir, filePNG, fileNB},
		dir =
        	Which[
        		SameQ[$InputFileName, ""],
        			Quiet @ Check[NotebookDirectory[], Directory[]]
        		,
        		StringContainsQ[$InputFileName, "WolframLanguageForJupyter"],
        			Directory[]
        		,
        		True,
        			DirectoryName[$InputFileName]
        	];

		sciWolframImageDir = FileNameJoin[{dir, "tmp", "wolfram"}];

		If[Not @ DirectoryQ[sciWolframImageDir],
			CreateDirectory[sciWolframImageDir, CreateIntermediateDirectories -> True]
		];
		filePNG = FileNameJoin[{sciWolframImageDir, StringTemplate["`1`.png"][CreateUUID["wolfram-"]]}];
		Export[filePNG, Notebook[{Cell[BoxData @ ToBoxes @ expr, "Output"]}], ImageResolution -> sciWolframImageDPI];
		Switch[sciWolframEnv,
			"emacs",
				WriteString["stdout", StringTemplate[": Out[`1`]= "][n++], "\n"];

				WriteString["stdout", StringTemplate["[[file:`1`]]"][filePNG], "\n"]
			,
			"vscode",
				Run[StringTemplate["imgcat `1`"][filePNG]];
		];
		If[playNB == "yes",
			fileNB = StringReplace[filePNG, ".png" -> ".nb"];
			Export[fileNB, Notebook[{Cell[BoxData @ ToBoxes @ expr, "Output"]}]];
                        If[StringQ @ Environment["WSL_DISTRO_NAME"],
                                If[StringQ[sciWolframPlayer],
                                    StartProcess[{sciWolframPlayer, FileNameTake[fileNB]}, ProcessDirectory -> sciWolframImageDir]
                                    ,
                                    WriteString["stdout", "Wolfram Player or Mathematica not found"]
                                ];
                        	,
                        	UsingFrontEnd @ SystemOpen[fileNB];
                        ];
		];
	];

Options[sciWolframDisplayImage] =
	{
		sciWolframFormulaType -> "image"
		,
		sciWolframImageDPI -> 100
		,
		sciWolframPlay -> "no"
	};

sciWolframDisplayImage[expr_, OptionsPattern[]] :=
	Module[{box, isFormula, isPlot},
		box = ToBoxes[expr];
		isFormula = Not @ FreeQ[box, RowBox | SqrtBox | SuperscriptBox];
		isPlot = Not @ FreeQ[box, DynamicBox | DynamicModuleBox | GraphicsBox | Graphics3DBox];
		Which[
			Not @ isFormula && Not @ isPlot,
				Switch[sciWolframEnv,
					"emacs",
						If[SameQ[expr, Null],
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
				Switch[sciWolframEnv,
					"emacs",
						Switch[OptionValue @ sciWolframFormulaType,
							"latex",
								If[isPlot,
									sciWolframImage[
										expr
										,
										OptionValue @ sciWolframImageDPI
										,
										playNB = OptionValue @ sciWolframPlay
									]
									,
									sciWolframTeX[expr]
								]
							,
							"image",
								sciWolframImage[
									expr
									,
									OptionValue @ sciWolframImageDPI
									,
									playNB =
										If[isPlot,
											OptionValue @ sciWolframPlay
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
							OptionValue @ sciWolframImageDPI
							,
							playNB =
								If[isPlot,
									OptionValue @ sciWolframPlay
									,
									"no"
								]
						]
				]
		]
	];

End[];

EndPackage[];
