#!/usr/bin/env wolframscript
returnToMenu=0;
linkKernel=LinkCreate[ LinkProtocol -> "TCPIP"];
linknumber1=StringSplit[linkKernel[[1]], {"@"}][[1]];
UseFrontEnd[CreateDialog[DialogNotebook[
    DynamicModule[{linknumber1 = linknumber1}, 
     Column[{Button[
        TextCell["New sphere mesh\nfrom existing model", Gray, 24, 
         Bold], linkFrontEnd = 
         LinkConnect[linknumber1, LinkProtocol -> "TCPIP"];
        LinkRead[linkFrontEnd]; LinkWrite[linkFrontEnd, 1]; 
        DialogReturn[], ImageSize -> {500, 120}, Alignment -> Center],
        Button[TextCell["New sphere mesh\nfrom .wdx file", Gray, 24, 
         Bold], linkFrontEnd = 
         LinkConnect[linknumber1, LinkProtocol -> "TCPIP"];
        LinkRead[linkFrontEnd]; LinkWrite[linkFrontEnd, 2]; 
        DialogReturn[], ImageSize -> {500, 120}, Alignment -> Center],
        Button[TextCell["View and edit\n.wdx file", Gray, 24, Bold], 
        linkFrontEnd = 
         LinkConnect[linknumber1, LinkProtocol -> "TCPIP"];
        LinkRead[linkFrontEnd]; LinkWrite[linkFrontEnd, 3]; 
        DialogReturn[], ImageSize -> {500, 120}, Alignment -> Center],
        Button[TextCell["New sphere mesh\ndirectly from .txt", Gray, 
         24, Bold], 
        linkFrontEnd = 
         LinkConnect[linknumber1, LinkProtocol -> "TCPIP"];
        LinkRead[linkFrontEnd]; LinkWrite[linkFrontEnd, 4]; 
        DialogReturn[], ImageSize -> {500, 120}, Alignment -> Center],
        Button[TextCell["Quit", Gray, 24, Bold], 
        linkFrontEnd = 
         LinkConnect[linknumber1, LinkProtocol -> "TCPIP"];
        LinkRead[linkFrontEnd]; LinkWrite[linkFrontEnd, 0]; 
        DialogReturn[], ImageSize -> {500, 120}, 
        Alignment -> Center]}], WindowTitle -> "SphereMesh1.0", 
     WindowFrameElements -> {}]]]];
LinkWrite[linkKernel,0];
startUpChoice= LinkRead[linkKernel];
LinkClose[linkKernel];