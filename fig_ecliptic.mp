verbatimtex
%&latex
\documentclass[12pt]{article}
  \usepackage{wasysym}
\begin{document}
etex

input astro
input textpath

%outputtemplate := "%j.mps";

beginfig(0);
  draw fullcircle scaled 2r;
  draw_equator(1,1);
  draw_ecliptic(1,1);
  draw_lunar_orbit(1,1);
  draw_meridian(0,1,1);
  
  z0=(0,0); % origin
  z1=project((r,0,0),V1,V2); % x-axis
  z2=project((0,r,0),V1,V2); % y-axis
  z3=project((0,0,r),V1,V2); % z-axis
  z4=project(North_Ec,V1,V2);
  z5=project(f_ecliptic(r,Ln),V1,V2);
  z6=project(f_ecliptic(r,75),V1,V2);
  z7=project(f_equator(r,70),V1,V2);
  z8=project(f_ecliptic(r,310),V1,V2);
  z9=project(f_ecliptic(r,315),V1,V2);

  drawarrow z0--0.2z1; label.bot(btex $x$ etex, 0.2z1);
  drawarrow z0--0.2z2; label.rt(btex $y$ etex, 0.2z2);
  drawarrow z0--0.2z3; label.lft(btex $z$ etex, 0.2z3);
  draw -z1--z1 dashed evenly;
  draw -z3--z3 dashed evenly;

  pickup pencircle scaled 2pt;
  dotlabel.urt(btex $\vernal$ etex, z1);
  dotlabel.bot(btex $\libra$ etex, -z1);
  dotlabel.llft(btex $N$ etex, z3);
  dotlabel.urt(btex $S$ etex, -z3);
  dotlabel.llft(btex $N^*$ etex, z4);
  dotlabel.urt(btex $S^*$ etex, -z4);
  dotlabel.lrt(btex $\ascnode$ etex, z5);

  pickup defaultpen;
  label.rt(btex Celestial equator etex, 1.01z7);
  label.rt(btex Ecliptic etex, 1.01z6);
  label.rt(btex Lunar orbit etex, 1.02*project(f_lunar(r,70),V1,V2));
  drawarrow z8..z9;
endfig;
end
