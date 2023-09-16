#!/usr/bin/env wolframscript
Needs["sphereMeshCore`", 
  FileNameJoin[{SphereMeshPath, "MMAscripts", "core.m"}]];
state = Catch[
   wdxPath = 
    UseFrontEnd[
     SystemDialogInput["FileOpen", ".wdx", 
      WindowTitle -> "the .wdx file"]];
   If[wdxPath === $Canceled, Throw[0]];
   impWDX[wdxPath];
   While[problemQ && choiceImpWDX,
    problemQ = False;
    WDXPath = 
     UseFrontEnd[
      SystemDialogInput["FileOpen", ".wdx", 
       WindowTitle -> "the .wdx file"]];
    If[txtPath === $Canceled, Throw[0]];
    impTXT[txtPath];];
   If[problemQ, Throw[0]];
   decision = review["Confirm", "Cancel"];
   If[decision == 2, Throw[0]]; 1];

