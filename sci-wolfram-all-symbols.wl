dir = $ScriptCommandLine[[2]];

(* Function to convert Wolfram symbol files to Emacs Lisp format *)

sciwolframSymbols[fileName_, splitNum_] :=
  Module[{symbol, symbolNum, splitSymbolNum},
    symbol = Import[FileNameJoin[{dir, "Data", fileName <> ".wl"}]];
    symbolNum = Length[symbol];
    splitSymbolNum = Quotient[symbolNum, splitNum];
    symbol =
      If[Mod[symbolNum, splitNum] == 0,
        Partition[symbol, splitSymbolNum]
        ,
        Join[Partition[Drop[symbol, -splitSymbolNum - Mod[symbolNum, 
          splitNum]], splitSymbolNum], {Drop[symbol, splitSymbolNum * (splitNum
           - 1)]}]
      ];
    symbol =
      MapThread[
        StringJoin[
          "(defvar "
          ,
          fileName
          ,
          If[splitNum > 1,
            "-" <> ToString[#1]
            ,
            ""
          ]
          ,
          " nil)\n"
          ,
          "(setq "
          ,
          fileName
          ,
          If[splitNum > 1,
            "-" <> ToString[#1]
            ,
            ""
          ]
          ,
          " '"
          ,
          StringRiffle[ToString[#, InputForm]& /@ #2, {"(", "\n", ")"
            }]
          ,
          ")"
        ]&
        ,
        {Range[splitNum], symbol}
      ];
    symbol = StringJoin[";;; ", fileName, ".el -*- lexical-binding: t -*-\n\n",
       ";;; Commentary:\n", ";;; Code:\n\n", StringRiffle[symbol, "\n\n"], 
      "\n\n(provide '", fileName, ")\n", ";;; ", fileName, ".el ends here"]
      ;
    Export[FileNameJoin[{dir, "Data", fileName <> ".el"}], symbol, "Text"
      ];
    Print[StringJoin["Conversion completed: ", fileName, ".wl -> ", fileName,
       ".el"]];
  ]

(* Convert the symbol files *)

sciwolframSymbols["BuiltinFunctions", 5];

sciwolframSymbols["Constants", 1];

sciwolframSymbols["SystemLongNames", 1];

sciwolframSymbols["SpecialLongNames", 1];

sciwolframSymbols["UndocumentedLongNames", 1];

sciwolframSymbols["FreeLongNames", 1];

sciwolframSymbols["SystemCharacters", 1];

sciwolframSymbols["SpecialCharacters", 1];

sciwolframSymbols["UndocumentedCharacters", 1];

sciwolframSymbols["FreeCharacters", 1];

sciwolframSymbols["Options", 1];

sciwolframSymbols["SessionSymbols", 1];

sciwolframSymbols["ExperimentalSymbols", 1];

sciwolframSymbols["UndocumentedSymbols", 1];

sciwolframSymbols["ObsoleteSymbols", 1];

sciwolframSymbols["BadSymbols", 1];

sciwolframSymbols["UnsupportedCharacters", 1];

sciwolframSymbols["UnsupportedLongNames", 1];
