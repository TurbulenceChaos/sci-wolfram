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
sciWolframPlay		-> \"yes\" or \"no\" (default) to convert plots to Mathematica interactive file,
sciWolframShortLines    -> 10 (default): Long expression are displayed using Short[expr, n], where n is the maximum number of lines to show
] &;
Tyep below code to reset $Post:
$Post = .
";

Begin["`Private`"];

(* Initialize output counter *)

n = 1;

sciWolframEnv =
	If[SameQ[Environment["TERM_PROGRAM"], "vscode"],
		"vscode"
		,
		"emacs"
	];

sciWolframPlayer = First[FileNames[{"*wolframplayer*", "*WolframNB*"}, $InstallationDirectory, 2], Null];

(* Display plain text output *)

sciWolframText[expr_, sciWolframShortLines_] :=
	Module[{},
		WriteString["stdout", StringTemplate[": Out[`1`]= "][n++], "\n"];
		WriteString["stdout", Short[expr, sciWolframShortLines], "\n"];
		(* Return orginal value for % calc in REPL *)
		expr;
	];

(* Display LaTeX output *)

sciWolframTeX[expr_, sciWolframShortLines_] :=
	Module[{},
		WriteString["stdout", StringTemplate[": Out[`1`]= "][n++], "\n"];
		WriteString["stdout", "\\begin{equation*}", "\n"];
		WriteString["stdout", TeXForm[Short[expr, sciWolframShortLines]], "\n"];
		WriteString["stdout", "\\end{equation*}", "\n"];
		expr;
	];

(* Display Image output *)

sciWolframImage[expr_, sciWolframImageDPI_, playNB_, sciWolframShortLines_] :=
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
		Export[filePNG, Notebook[{Cell[BoxData @ ToBoxes @ Short[expr, sciWolframShortLines], "Output"]}], ImageResolution -> sciWolframImageDPI];
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
			Export[fileNB, Notebook[{Cell[BoxData @ ToBoxes @ Short[expr, sciWolframShortLines], "Output"]}]];
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
		expr;
	];

Options[sciWolframDisplayImage] =
	{
		sciWolframFormulaType -> "image"
		,
		sciWolframImageDPI -> 100
		,
		sciWolframPlay -> "no"
		,
		sciWolframShortLines -> 10
	};

systemBoxSymbols = Apply[Alternatives, ToExpression @ Select[Names["*Box"], StringFreeQ[{"RowBox", "InterpretationBox"}]]];

systemGraphicsBoxSymbols = Apply[Alternatives, ToExpression @ Names[{"Graphics*Box","Dynamic*Box"}]];

sciWolframDisplayImage[expr_, OptionsPattern[]] :=
	Module[{box, isString, isPlot},
		box = ToBoxes[expr];
		isString = FreeQ[box, systemBoxSymbols | Cell];
		isPlot = Not @ FreeQ[box, systemGraphicsBoxSymbols];
		Which[
			isString,
				Switch[sciWolframEnv,
					"emacs",
						If[SameQ[expr, Null],
							expr
							,
							sciWolframText[expr, OptionValue @ sciWolframShortLines]
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
										,
										OptionValue @ sciWolframShortLines
									]
									,
									sciWolframTeX[expr, OptionValue @ sciWolframShortLines]
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
									,
									OptionValue @ sciWolframShortLines
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
							,
							OptionValue @ sciWolframShortLines
						]
				]
		]
	];

End[];

EndPackage[];
