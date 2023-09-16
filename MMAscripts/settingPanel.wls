#!/usr/bin/env wolframscript
linkKernel = LinkCreate[LinkProtocol -> "TCPIP"];
linknumber4 = StringSplit[linkKernel[[1]], {"@"}][[1]];
UseFrontEnd[
 w = CreateWindow[
   DialogNotebook[
    DynamicModule[{chosenAccuracy = 3, inputAccuracy = 0, 
      chosenMethod = 1, supportingOn = {1}, inputN = 0.1, 
      inputDepth = 0, inputSupportingsNumber = 2, wdxOn = {1}, 
      linknumber4 = linknumber4}, 
     Column[{TextCell["  Accuracy:", 18, Black, Bold], 
       RadioButtonBar[
        Dynamic[chosenAccuracy], {1 -> 
          TextCell["Extremely fine", 24, Bold, Gray], 
         2 -> TextCell["Fine", 24, Bold, Gray], 
         3 -> TextCell["Medium (recommended)", 24, Bold, Gray], 
         4 -> TextCell["Rough", 24, Bold, Gray], 
         5 -> TextCell["Extremely rough", 24, Bold, Gray], 
         0 -> Column[{TextCell["customized ", 24, Bold, Gray], 
            Row[{TextCell["number of random points: ", 12, Bold, 
               Gray], InputField[Dynamic[inputAccuracy], 
               ImageSize -> {140, 20}, 
               Enabled -> Dynamic[chosenAccuracy == 0]]}]}]}, 
        Appearance -> "Vertical"], TextCell["\n", 24, Bold, Gray], 
       Row[{TextCell["d<r1+r2 and (1-n)*(r1+r2)<d   n:", 20, Gray, 
          Bold], InputField[Dynamic[inputN], ImageSize -> {100, 20}, 
          Enabled -> Dynamic[chosenMethod == 1]]}], 
       TextCell["\n", 24, Bold, Gray], 
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
       Row[{Button["Confirm", 
          linkFrontEnd = 
           LinkConnect[linknumber4, LinkProtocol -> "TCPIP"]; 
          LinkWrite[
           linkFrontEnd, {chosenAccuracy, inputAccuracy, chosenMethod,
             inputN, inputDepth, supportingOn, inputSupportingsNumber,
             wdxOn}], ImageSize -> {80, 40}], 
         Button["Cancel", 
          linkFrontEnd = 
           LinkConnect[linknumber4, LinkProtocol -> "TCPIP"]; 
          LinkWrite[linkFrontEnd, $Canceled], 
          ImageSize -> {80, 40}]}]}]], 
    WindowTitle -> "New Sphere Mesh", WindowFrameElements -> {}, 
    WindowSize -> {500, 700}]]]
settings = LinkRead[linkKernel];
UseFrontEnd[NotebookClose[w]];
LinkClose[linkKernel];