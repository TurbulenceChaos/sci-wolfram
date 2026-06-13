(* Display wolfram script image *)

BeginPackage["sciWolframDisplayImage`"];

sciWolframDisplayImage::usage = "Display wolfram script image.
Usage:
Default:
$Post = sciWolframDisplayImage[#]&;
All options:
$Post = sciWolframDisplayImage[#,
sciWolframEnv		-> \"emacs\" or \"vscode\",
sciWolframFormulaType	-> \"image\" or \"latex\",
sciWolframImageDir	-> \"image output direcory\",
sciWolframImageDPI	-> 150,
sciWolframPlay		-> \"yes\" or \"no\"
] &;
Tyep below code restore $Post original value:
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

(* Display Image output *)

sciWolframImage[expr_, sciWolframEnv_, sciWolframImageDir_, sciWolframImageDPI_,
	 playCDF_] :=
	Module[{filePNG, fileCDF},
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
		If[playCDF == "yes",
			fileCDF = StringReplace[filePNG, ".png" -> ".cdf"];
			Export[fileCDF, Notebook[{Cell[BoxData @ ToBoxes @ expr, "Output"]}]];
			UsingFrontEnd @ SystemOpen[fileCDF];
		];
	];

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

Options[sciWolframDisplayImage] =
	{
		sciWolframEnv ->
			If[SameQ[Environment["TERM_PROGRAM"], "vscode"],
				"vscode"
				,
				"emacs"
			]
		,
		sciWolframFormulaType -> "image"
		,
		sciWolframImageDir -> FileNameJoin[{dir, "tmp", "wolfram"}]
		,
		sciWolframImageDPI ->
			If[SameQ[Environment["TERM_PROGRAM"], "vscode"],
				100
				,
				150
			]
		,
		sciWolframPlay -> "no"
	};

sciWolframDisplayImage[expr_, OptionsPattern[]] :=
	Module[{box, isFormula, isPlot},
		box = ToBoxes[expr];
		isFormula = Not @ FreeQ[box, RowBox | SqrtBox | SuperscriptBox];
		isPlot = Not @ FreeQ[box, DynamicBox | DynamicModuleBox | GraphicsBox
			 | Graphics3DBox];
		Which[
			Not @ isFormula && Not @ isPlot,
				Switch[OptionValue @ sciWolframEnv,
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
				Switch[OptionValue @ sciWolframEnv,
					"emacs",
						Switch[OptionValue @ sciWolframFormulaType,
							"latex",
								If[isPlot,
									sciWolframImage[
										expr
										,
										OptionValue @ sciWolframEnv
										,
										OptionValue @ sciWolframImageDir
										,
										OptionValue @ sciWolframImageDPI
										,
										playCDF = OptionValue @ sciWolframPlay
									]
									,
									sciWolframTeX[expr]
								]
							,
							"image",
								sciWolframImage[
									expr
									,
									OptionValue @ sciWolframEnv
									,
									OptionValue @ sciWolframImageDir
									,
									OptionValue @ sciWolframImageDPI
									,
									playCDF =
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
							OptionValue @ sciWolframEnv
							,
							OptionValue @ sciWolframImageDir
							,
							OptionValue @ sciWolframImageDPI
							,
							playCDF =
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
