#!/usr/bin/env wolframscript
 Needs["sphereMeshCore`", 
  FileNameJoin[{SphereMeshPath, "MMAscripts", "core.m"}]];
state = Catch[
   stlPath = 
    UseFrontEnd[
     SystemDialogInput[
      "FileOpen", {"model", {"CAD files" -> {"*.stl", "*.dxf", 
          "*.zpr"}}}, WindowTitle -> "the model"]];
   If[stlPath === $Canceled, Throw[0]];
   txtPath = 
    UseFrontEnd[
     SystemDialogInput["FileOpen", ".txt", 
      WindowTitle -> "the radius field"]];
   If[txtPath === $Canceled, Throw[0]];
   imprf[txtPath];
   While[problemQ && choiceImprf, problemQ = False;
    txtPath = 
     UseFrontEnd[
      SystemDialogInput["FileOpen", ".txt", 
       WindowTitle -> "the radius field"]] ; 
    If[txtPath === $Canceled, Throw[0]];
    imprf[txtPath];];
   If[problemQ, Throw[0]];
   impstl[stlPath];
   While[problemQ && choiceImpstl, problemQ = False;
    stlPath = 
     UseFrontEnd[
      SystemDialogInput[
       "FileOpen", {"model", {"CAD files" -> {"*.stl", "*.dxf", 
           "*.zpr"}}}, WindowTitle -> "the model"]];
    If[stlPath === $Canceled, Throw[0]];
    impstl[stlPath];];
   If[problemQ, Throw[0]];
   
   SetDirectory[FileNameJoin[{SphereMeshPath, "MMAscripts"}]];
   << "settingPanel.wls";
   If[settings === $Canceled, Throw[0]];
   {chosenAccuracy, inputAccuracy, chosenMethod, inputN, inputDepth, 
     supportingOn, inputSupportingsNumber, wdxOn} = settings;
   Which[chosenAccuracy == 1, 
    numberOfPionts = 
     Volume[stl]/((Xmax - Xmin) (Ymax - Ymin) (Zmax - Zmin))*625000, 
    chosenAccuracy == 2, 
    numberOfPionts = 
     Volume[stl]/((Xmax - Xmin) (Ymax - Ymin) (Zmax - Zmin))*64000, 
    chosenAccuracy == 3, 
    numberOfPionts = 
     Volume[stl]/((Xmax - Xmin) (Ymax - Ymin) (Zmax - Zmin))*27000, 
    chosenAccuracy == 4, 
    numberOfPionts = 
     Volume[stl]/((Xmax - Xmin) (Ymax - Ymin) (Zmax - Zmin))*8000, 
    chosenAccuracy == 5, 
    numberOfPionts = 
     Volume[stl]/((Xmax - Xmin) (Ymax - Ymin) (Zmax - Zmin))*6250, 
    chosenAccuracy == 0, numberOfPionts = inputAccuracy];
   SUPPORTINGS = {};
   numberOfPionts = Floor[numberOfPionts];
   If[numberOfPionts < 10, numberOfPionts = 10];
   createrf[];
   If[inputN <= 0, inputN = 0.001];
   If[inputN > 0.5, inputN = 0.5];
   BALLS = b[rf, inputN];
   
   While[True, 
    If[Length[supportingOn] > 0, d = N[(dx dy dz/Length[BALLS])^(1/3)];
     xdiv = 10 (Floor[(Xmax - Xmin)/d] + 1);
     ydiv = 10 (Floor[(Ymax - Ymin)/d] + 1);
     zdiv = 10 (Floor[(Zmax - Zmin)/d] + 1);
     SUPPORTINGS = 
      addsupportingsArch[{Xmin, Xmax, Ymin, Ymax, Zmin, Zmax}, {xdiv, 
        ydiv, zdiv}, BALLS, 1, inputSupportingsNumber];];
    If[Length[supportingOn] == 0, 
     SUPPORTINGS = 
      Table[{Append[BALLS[[numm]], numm], {}}, {numm, 1, 
        Length[BALLS]}]];
    returned = adjust[];
    If[returned[[1]] === "finish", 
     If[returned[[2]] === $Canceled, Throw[0], Break[]]]; 
    BALLS = returned[[2]];];
   
   finalDecision = 1;
   savePath = $Canceled;
   If[(finalDecision == 1) && (Length[wdxOn] > 0), 
    savePath = 
      UseFrontEnd[
       SystemDialogInput[
        "FileSave", {"sphere_mesh", {"WDX" -> {"*.wdx"}}}, 
        WindowTitle -> "save to"]];];
   If[savePath === $Canceled&& (Length[wdxOn] > 0), Throw[0]];
  If[! savePath === $Canceled, 
 Export[savePath, {{}, {}, 
   SUPPORTINGS, {Xmaxt, Xmint, Ymaxt, Ymint, Zmaxt, Zmint}, "OKAY"}]]; 1];
