
#include <iostream>
#include <map>
#include <cstdint>

enum Tile{
  WHITE=0, BLACK
};

enum Direction{
  NORTH, SOUTH, EAST, WEST
};

typedef int32_t coord_t;

typedef std::pair<coord_t, coord_t> Point;

typedef std::map<Point, Tile> Grid;

template<class K, class T>
T getDefault(std::map<K,T>& map, K key, T defaultVal){
  auto itr = map.find(key);
  if(itr == map.end()){
    return defaultVal;
  }else{
    return (*itr).second;
  }
}

Direction clockwise(Direction start){
  switch(start){
  case NORTH: return EAST;
  case EAST: return SOUTH;
  case SOUTH: return WEST;
  case WEST: return NORTH;
  }
  return NORTH;
}

Direction antiClockwise(Direction start){
  switch(start){
  case NORTH: return WEST;
  case WEST: return SOUTH;
  case SOUTH: return EAST;
  case EAST: return NORTH;
  }
  return NORTH;
}

void move(Point& pt, Direction dir){
  switch(dir){
  case NORTH: ++pt.second; break;
  case SOUTH: --pt.second; break;
  case EAST:  ++pt.first; break;
  case WEST:  --pt.first; break;
  }
}

int main(){
  const uint64_t iterations = 1000000000000000000;
  uint64_t blacks = 0;
  Point ant(0,0);
  Direction dir = NORTH;
  Grid grid;
  for(uint64_t i = 0; i<iterations; ++i){
    std::cout << "Working...(" << (100.0*(i+1.0)/iterations)
	      << "%)\r" << std::flush;
    if(getDefault(grid, ant, WHITE) == WHITE){
      ++blacks;
      grid[ant] = BLACK;
      dir = clockwise(dir);
    }else{
      --blacks;
      grid[ant] = WHITE;
      dir = antiClockwise(dir);
    }
    move(ant, dir);
  }
  std::cout << "\nDONE! result = " << blacks << std::endl;
  return 0;
}
