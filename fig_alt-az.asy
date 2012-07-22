size(300);
import texcolors;
import graph3;
import three;
import labelpath3;
import math;
texpreamble("\usepackage{bm}");
texpreamble("\usepackage{wasysym}");

//Draw right angle (MA,MB) in 3D
void drawrightangle(picture pic=currentpicture,
                    triple M, triple A, triple B,
                    real radius=0,
                    pen p=currentpen,
                    pen fillpen=nullpen,
                    projection P=currentprojection)
{
  p=linejoin(0)+linecap(0)+p;
  if (radius==0) radius=arrowfactor*sqrt(2);
  transform3 T=shift(-M);
  triple OA=radius/sqrt(2)*unit(T*A),
    OB=radius/sqrt(2)*unit(T*B),
    OC=OA+OB;
  path3 _p=OA--OC--OB;
  picture pic_;
  draw(pic_, _p, p=p);
  if (fillpen!=nullpen) draw(pic_, surface(O--_p--cycle), fillpen);
  add(pic,pic_,M);
}

//currentprojection=perspective(5,2,2);
currentprojection=perspective(5,2,1.5);

real a=1;
real b=a;
real c=0.9;

pen bg=gray(0.9)+opacity(0.5);

// (Parametrized) Ellipsoid with equatorial radii a and
// b, and polar radius c
// t is lat, lon
triple ellipsoid(pair t) {
  return (a*cos(t.x)*cos(t.y),b*cos(t.x)*sin(t.y),c*sin(t.x));
}

// Ellipsoid in cartesian coordinates with axis-aligned radii
// x^2/a^2 + y^2/b^2 + z^2/c^2 = 1
//triple ellipsoid(triple) {
//}


pen p=rgb(0,0.7,0);
draw(Label("$i$",1),O--0.5*X,1.5E+0.2S,p,Arrow3);
draw(Label("$j$",1),O--0.5*Y,p,Arrow3);
draw(Label("$k$",1),O--0.5*Z,p,Arrow3);
label("$C$",(0,0,0),0.9W+0.2N);
//axes3("$i$","$j$","$k$",Arrow3);

//ellipsoid
pair latRange=(0,pi/2);
pair lonRange=(-pi/30,pi/2+pi/30);
surface s=surface(ellipsoid,(latRange.x,lonRange.x),(latRange.y,lonRange.y),Spline);
draw(s,lightgray+opacity(0.2));

//x-z
//draw(surface((1.2,0,0)--(1.2,0,1.2)--(0,0,1.2)--(0,0,0)--cycle),bg,bg);
//y-z: this one should be rotated arounf z to match P
//draw(surface((0,1.2,0)--(0,1.2,1.2)--(0,0,1.2)--(0,0,0)--cycle),bg,bg);
//x-y
surface xyPlane=surface((1.2,0,0)--(1.2,1.2,0)--(0,1.2,0)--(0,0,0)--cycle);
draw(xyPlane,bg,bg);


// Point P
real lat, lon;
lat=50; lon=60;
triple pP=ellipsoid((radians(lat),radians(lon)));
draw("$\rho$",O--pP,N);
label("$P$",pP,N+W);


int steps=20;
real deltaLat=latRange.y/steps;
guide3 meridian;
// meridian thru pP
for(int i=0; i <= steps; ++i) {
  meridian=meridian..ellipsoid((i*deltaLat,radians(lon)));
}
draw(meridian,dashed);

// greenwich meridian
meridian=nullpath3;
for(int i=0; i <= steps; ++i) {
  meridian=meridian..ellipsoid((i*deltaLat,0));
}
string txt1="\small Greenwich meridian";
draw(labelpath(txt1,subpath(meridian,1,reltime(meridian,0.40)),angle=90),orange);
draw(meridian);

// equator
real deltaLon=lonRange.y/steps;
guide3 equator;
for(int i=0; i <= steps; ++i) {
  equator=equator..ellipsoid((0,i*deltaLon));
}
string txt1="\small equator";
draw(labelpath(txt1,subpath(equator,reltime(equator,0.05),reltime(equator,0.16)),angle=180),orange);
draw(equator);

// Celestial body
triple cb=pP+(0.05,0.8,0.55);
real width=1;
dot("$B$",cb,SE,linewidth(width));
//draw("$\rho$",(0,0,0)--cb,Arrow3,PenMargin3(0,width));
draw("$d$",pP--cb,Arrow3);

// Plane tangent to ellipsoid in P
// x*x_p/a^2 + y*y_p/b^2 + z*z_p/c^2 + 1 = 0
real zOfPerpendicularPlane(pair p) {
  return (1-p.x*pP.x/a^2-p.y*pP.y/b^2)*(c^2)/pP.z;
}

//plane tanget to ellipsoid in P
pair pp[]={(-0.2,-0.2), (0.8,-0.2), (0.8,1.2), (-0.2,1.2)};
path3 tanplane=(pp[0].x,pp[0].y,zOfPerpendicularPlane(pp[0]))--
  (pp[1].x,pp[1].y,zOfPerpendicularPlane(pp[1]))--
  (pp[2].x,pp[2].y,zOfPerpendicularPlane(pp[2]))--
  (pp[3].x,pp[3].y,zOfPerpendicularPlane(pp[3]))--cycle;
draw(surface(tanplane),yellow+opacity(0.1));

// perpendicular to plane tanget in P
//triple perpP=dir(pP.x/a^2,pP.y/b^2,pP.z/c^2);
triple perpP=normal(tanplane);

// celestial body projection onto tangent plane
triple pppp=planeproject(tanplane)*cb;
draw(pP--pppp,dashed);
draw(pppp--cb,dashed);
drawrightangle(pppp,pP,cb,fillpen=lightgray);

// lat-lon arcs
triple pA=(pP.x,pP.y, 0);
//draw(pA--pP,dashed);
draw("$\varphi'$", reverse(arc(O,0.4*unit(pP),0.4*unit(pA))), E, Arrow3);
draw("$L$", arc(O,0.4*X,0.4*unit(pA)), S+0.3E,Arrow3);


// Moving axes
// i see below
//triple k=unit(pP);
triple k=normal(tanplane);
// triple i=dir(colatitude(pP-centerGravity),lon);//the normal (alternative)
draw(Label("\footnotesize $Zenith$",1),pP--pP+0.2*k,0.9N,red,Arrow3);


// center of gravity
triple[] ps=intersectionpoints(pP-k--pP, xyPlane);
triple centerGravity=ps[0];
draw(centerGravity--pP,N,red);
draw(O--ellipsoid((0,radians(lon))),dashed);
dot("$O$",centerGravity,W+0.2S);
draw("$\varphi$",
     shift(centerGravity)*
     reverse(arc(O,0.12*unit(pP-centerGravity),0.12*unit(pA-centerGravity))),
     E,Arrow3);


triple i=dir(colatitude(pP-centerGravity)+90,lon);
draw(Label("\footnotesize $South$",1),pP--pP+0.4*i,SSE,red,Arrow3);

triple j=cross(k,i);
//draw(Label("\footnotesize $North$",1),pP--pP-0.2*i,N,red,Arrow3);
draw(Label("\footnotesize $West$",1),pP--pP-0.2*j,W,red,Arrow3);
dot("$N$",ellipsoid((radians(90),0)),N);

// azimuth, elevation
draw("$A$",shift(pP)*(arc(pP,0.2*unit(i),0.2*unit(pppp-pP))),SE,Arrow3);
draw("$h$",shift(pP)*(arc(O,0.2*unit(pppp-pP),0.2*unit(cb-pP))),SE,Arrow3);

drawrightangle(pP,pP+i,centerGravity,fillpen=lightgray);
