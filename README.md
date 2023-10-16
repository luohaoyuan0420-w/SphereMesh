# SphereMesh
<img src="https://github.com/luohaoyuan0420-w/SphereMesh/blob/main/demo1.png" alt="demo1" height="180px" width="200"></img>
<img src="https://github.com/luohaoyuan0420-w/SphereMesh/blob/main/demo1-printed.jpg" alt="demo1-printed" height="300px" width="200"></img>
<img src="https://github.com/luohaoyuan0420-w/SphereMesh/blob/main/demo2.jpg" alt="demo2" height="300px" width="200"></img>
<img src="https://github.com/luohaoyuan0420-w/SphereMesh/blob/main/demo3.jpg" alt="demo3" height="300px" width="200"></img>

This is a program created to generate 3D-printable tangential spherical meshes inside an arbitrary shape. Along with an STL file that describes the shape in which we wish to stuff these spherical meshes, a TXT file of control points should also be given to specify the overall radii distribution of these spherical meshes.   

It is believed that spherical shell stuffing can increase the strength of mechanical parts and at the same time lower their weights. Holes are opened on these spherical shells making them into meshes in order to enable metal powders to get out of these shells in the situation of metal 3D-printing. Because floating objects cannot be printed, supporting structures have to be added. The usual supporting structures have sharp edges and might puncture these spherical shells breaking the mechanical properties of the model. Therefore, in the program one can choose to add arcuate supporting structures that are also tangential to these spherical shells.   

To run this program one has to first install Mathematica or Wolfram Kernel and then UGNX. The tested versions are 12.0 for Mathematica or Wolfram Kernel and 10.0 for UGNX. Execute the binary "SphereMesh1.0.dll" in UGNX by shortcut "Ctrl+U". The format of control points given in the TXT file is {x, y, z, r_out, r_in, n_hole, r_hole, d_support}, and the TXT file must contain at least two control points. "r" represents radius and "d" represents diameter. One line for one point and columns are separated by a single space. 


本科的时候做的一个小项目。UGNX的源代码似乎已经找不到了。具体使用方法参见《SpherelMesh1.0使用说明.pdf》。
