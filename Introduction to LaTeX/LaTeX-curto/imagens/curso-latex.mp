prologues:= 3;
filenametemplate "%j-%c.mps";

beginfig(1)

  u:=0.5cm;
  ph:=4u;
  oph:=3u;
  pskip:=2u;
  pi:= 1.5u;
  pw:=15u;

  %pickup pencircle scaled 1pt;

  x0=x1=x6=x7=0;
  x2=x3=x8=x9=pw;
  x4=x5=x11=x10=pi;

  y7=y8=0u;
  y6+u=y11+u=y9=y10=oph;
  y3=y4=y0+u=y5+u=y1+ph=y2+ph=oph+pskip+ph;

  path p[];

  p1:= z0--z1--z2--z3--z4--z5--cycle;
  p2:= z6--z7--z8--z9--z10--z11--cycle;

  draw p1;
  draw p2;

  x12=x0;
  x13=x5;
  y12=y13=0.5(y4+y5);

  drawdblarrow z12--z13;

  z14 = 0.5[z12,z13];
  x15 = x14;
  y14+1.1u=y15;

  draw z14--z15;

  label.top(btex \tt\string\parindent etex, z15);

  picture pic;

  z16 = 0.5[z1,z2];
  x17=x16;
  y17=y10;

  pic:= thelabel(btex \tt\string\parskip etex,0.5[z16,z17]);

  z18 = whatever[z16,z17]=whatever[ulcorner pic,urcorner pic];
  z19 = whatever[z16,z17]=whatever[llcorner pic,lrcorner pic];

  drawarrow z18--z16;
  drawarrow z19--z17;
  draw pic;
  

  z20=whatever[z1,z3]=whatever[z16,z17];
  z21=whatever[z7,z9]=whatever[z16,z17];

  z22=z20+(u,-u);
  
  label(btex par\'agrafo etex,z22);
  label(btex outro par\'agrafo etex,z21);

  % baselineskip

  y23=y25=y5=y28+u=y26+u=y24+u;
  x28=x5=x23-u=x24-u=x25-0.5u=x26-0.5u;
  z29=0.5[z25,z26]=z27-(u,0);

  drawdblarrow z25--z26;
  draw z27--z29;
  draw z5--z23 dashed evenly;
  draw z24--z28 dashed evenly;

  label.rt(btex \tt\string\baselineskip etex,z27);
  
endfig;

end;