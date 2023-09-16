#!/usr/bin/env wolframscript

Needs["JLink`"];
$FrontEndLaunchCommand = 
  FileNameJoin[{$InstallationDirectory, "Mathematica.exe"}];
 fronendstate = ConnectToFrontEnd[];
If [fronendstate == True,
 state = Catch[
   list1 = {"MiniSnap.dll", "MiniSnap.xml", "ml32i4.dll", "ml64i4.dll", 
"MMAscripts", "NXOpen.dll", "NXOpen.UF.dll", "NXOpenUI.dll", 
"NXOpenUI.xml", "NXOpen.Utilities.dll", "NXOpen.Utilities.xml", 
"NXOpen.xml", "pskernel_net.dll", "Snap.dll", "Snap.xml", 
"SphereMesh1.0.dll", "SphereMesh1.0.pdb", "SphereMesh1.0.xml", 
"temp", "welcome.png", "Wolfram.NETLink.dll"};
   list2 = {"addBallsQ.wls", "adjust.wls", "buildBall.wls", 
"chooseWayToStart.wls", "close.wls", "core.m", "review.wls", 
"settingPanel2.wls", "settingPanel.wls", "start1.wls", "start2.wls", 
"start3.wls", "start4.wls", "startup.wls", "trimQ.wls"};
   SetDirectory[SphereMeshPath];
   If[! (And @@ 
       Join[Map[FileExistsQ, list1], 
        SetDirectory[FileNameJoin[{SphereMeshPath, "MMAscripts"}]];
        Map[FileExistsQ, list2]]), Throw[-1]];
   SetDirectory[$InstallationDirectory];
   If[! FileExistsQ["Mathematica.exe"], Through[-2]];
   SetDirectory[SphereMeshPath];
   welcome = Import["welcome.png"];
   UseFrontEnd[
    welcomeScreen = 
     CreatePalette[welcome, Saveable -> False, 
      WindowMargins -> {{180, Automatic}, {Automatic, 100}}, 
      WindowFloating -> True, WindowTitle -> "SphereMesh1.0"]];
   Pause[2.5];
   UseFrontEnd[NotebookClose[welcomeScreen]]; 1],
 state = -3;
 ]

