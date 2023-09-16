#!/usr/bin/env wolframscript
linkKernel = LinkCreate[LinkProtocol -> "TCPIP"];
linknumber4 = StringSplit[linkKernel[[1]], {"@"}][[1]];
UseFrontEnd[w = CreateWindow[
  DialogNotebook[
   DynamicModule[{chosenAccuracy = 3, inputAccuracy = 0, 
     chosenMethod = 1, supportingOn = {1}, inputN = 0.1, 
     inputDepth = 0, inputSupportingsNumber = 2, wdxOn = {1},  linknumber4 = linknumber4}, 
    Column[{ 
      TextCell["  Supportings:", 18, Black, Bold], 
      CheckboxBar[
       Dynamic[supportingOn], {1 -> 
         Column[{TextCell["Add supportings", 24, Bold, Gray], 
           Row[{TextCell["number of supportings each ball: ", 12, 
              Gray, Bold], 
             InputField[Dynamic[inputSupportingsNumber], 
              ImageSize -> {100, 20}, 
              Enabled -> Dynamic[Length[supportingOn] > 0]]}]}]}], 
      TextCell["\n\n", 12, Bold, Gray], 
      CheckboxBar[
       Dynamic[wdxOn], {1 -> 
         TextCell["Generate .wdx file", 18, Bold, Black]}], 
      TextCell["", 18, Bold, Black], 
      Row[{Button["Confirm",   linkFrontEnd = 
           LinkConnect[linknumber4, LinkProtocol -> "TCPIP"]; 
         LinkWrite[
          linkFrontEnd, {chosenAccuracy, inputAccuracy, chosenMethod, 
           inputN, inputDepth, supportingOn, inputSupportingsNumber, 
           wdxOn}], ImageSize -> {80, 40}], 
        Button["Cancel",   linkFrontEnd = 
           LinkConnect[linknumber4, LinkProtocol -> "TCPIP"]; LinkWrite[linkFrontEnd, $Canceled], 
         ImageSize -> {80, 40}]}]}]], 
   WindowTitle -> "New Sphere Mesh", WindowFrameElements -> {}, 
   WindowSize -> {500, 250}]]]
settings = LinkRead[linkKernel];
UseFrontEnd[NotebookClose[w]];