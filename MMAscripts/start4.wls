#!/usr/bin/env wolframscript
Needs["sphereMeshCore`", 
  FileNameJoin[{SphereMeshPath, "MMAscripts", "core.m"}]];
state = Catch[
   txtPath = 
    UseFrontEnd[
     SystemDialogInput["FileOpen", ".txt", 
      WindowTitle -> "the TXT file"]];
   If[txtPath === $Canceled, Throw[0]];
   impTXT[txtPath];
   While[problemQ && choiceImpTXT, problemQ = False;
    txtPath = 
     UseFrontEnd[
      SystemDialogInput["FileOpen", ".txt", 
       WindowTitle -> "the TXT file"]];
    If[txtPath === $Canceled, Throw[0]];
    impTXT[txtPath];];
   If[problemQ, Throw[0]];
   
   SetDirectory[FileNameJoin[{SphereMeshPath, "MMAscripts"}]];
   << "settingPanel2.wls";
   If[settings === $Canceled, Throw[0]];
   {chosenAccuracy, inputAccuracy, chosenMethod, inputN, inputDepth, 
     supportingOn, inputSupportingsNumber, wdxOn} = settings;
   SUPPORTINGS = {};
   Rmax = 1.5*Max[BALLS[[1 ;; -1, 4]]];
   Xmax = Max[BALLS[[1 ;; -1, 1]]] + Rmax; 
   Xmin = Min[BALLS[[1 ;; -1, 1]]] - Rmax;
   Ymax = Max[BALLS[[1 ;; -1, 2]]] + Rmax; 
   Ymin = Min[BALLS[[1 ;; -1, 2]]] - Rmax;
   Zmax = Max[BALLS[[1 ;; -1, 3]]] + Rmax; 
   Zmin = Min[BALLS[[1 ;; -1, 3]]] - Rmax;
   dx = (Xmax - Xmin); dy = (Ymax - Ymin); dz = (Zmax - Zmin);
   
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
   
   If[Length[supportingOn] > 0, d = N[(dx dy dz/Length[BALLS])^(1/3)];
    xdiv = 10 (Floor[(Xmax - Xmin)/d] + 1);
    ydiv = 10 (Floor[(Ymax - Ymin)/d] + 1);
    zdiv = 10 (Floor[(Zmax - Zmin)/d] + 1);
    SetDirectory[FileNameJoin[{SphereMeshPath, "MMAscripts"}]];
    SUPPORTINGS = 
     addsupportingsArch[{Xmin, Xmax, Ymin, Ymax, Zmin, Zmax}, {xdiv, 
       ydiv, zdiv}, BALLS, 1, inputSupportingsNumber];];
   If[Length[SUPPORTINGS] == 0, SUPPORTINGS = Map[{#, {}} &, BALLS]];
   
   
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
   SUPPORTINGS, {Xmax, Xmin, Ymax, Ymin, Zmax, Zmin}, "OKAY"}]]; 1];
