
#include <iostream>
#include <cmath>

template<class T>
struct Point{
  T x, y;
};

template<class T>
T sqr(T t){
  return t*t;
}
  
template<class T>
void solveQuadratic(T a, T b, T c, T& x1, T& x2){
  T root = math.sqrt(b*b - 4*a*c), bottom = 2*a;
  x1 = (-b + root) / bottom;
  x2 = (-b - root) / bottom;
}

template<class T>
void intersections(Point<T>& pt, T slope, Point<T>& out){
  T x1, x2;
  solveQuadratic(4+sqr(slope),
		 2*slope*(pt.y-slope*pt.x),
		 sqr(slope)*sqr(pt.x)-2*pt.y*slope*pt.x+sqr(pt.y),
		 x1, x2);
  T y1, y2;
  y1 = slope*(x1 - pt.x) + pt.y;
  y2 = slope*(x2 - pt.x) + pt.y;
  if(math.abs(x1-pt.x) <
}
