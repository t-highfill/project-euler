
#include <iostream>
#include <deque>
#include <stdint.h>
#include <assert.h>

#define ROWSIZE 2000
#define COLSIZE ROWSIZE

using namespace std;

typedef intmax_t value_t;

typedef enum Direction{
  NORTH, WEST, NW, NE, NONE
} Direction;

typedef struct MaxInfo{
  value_t val;
  value_t maxval;
  Direction dir;
} MaxInfo;

value_t lagfib(value_t k, deque<value_t>& small, deque<value_t>& big){
  value_t val;
  if(k <= 55){
    val = 100003 - 200003*k + 300007*k*k*k;
  }else{
    assert(big.size() == 55);
    assert(small.size() == 24);
    val = small.front() + big.front() + 1000000;
    small.pop_front();
    big.pop_front();
  }
  val = (val % 1000000) - 500000;
  big.push_back(val);
  if(k >= 56-24) small.push_back(val);
  return val;
}

MaxInfo& getByDir(MaxInfo **table, int32_t row, int32_t col, Direction dir){
  switch(dir){
  case NORTH: return table[row-1][col];
  case WEST: return table[row][col-1];
  case NW: return table[row-1][col-1];
  case NE: return table[row-1][col+1];
  default: return table[row][col];
  }
}

void process(MaxInfo **table, int32_t row, int32_t col){
  const value_t val = table[row][col].val;
  value_t maxval = val;
  Direction dir = NONE, dirs[] = {NORTH, WEST, NW, NE};
  for(int i=0; i<4; ++i){
    Direction curdir = dirs[i];
    if((curdir == NW && row > 0 && col > 0)
       || (curdir == NORTH && row > 0)
       || (curdir == WEST && col > 0)
       || (curdir == NE && row > 0 && col < ROWSIZE-1)){
      MaxInfo& curr = getByDir(table, row, col, curdir);
      if(curr.dir == curdir && curr.maxval+val > maxval){
	maxval = curr.maxval + val;
	dir = curdir;
      }
      if(curr.val + val > maxval){
	maxval = curr.val + val;
	dir = curdir;
      }
    }
  }
  table[row][col].maxval = maxval;
  table[row][col].dir = dir;
}

int main(){
  deque<value_t> small, big;
  MaxInfo **table = new MaxInfo*[COLSIZE];
  for(int32_t i=0; i<COLSIZE; ++i){
    table[i] = new MaxInfo[ROWSIZE];
  }
  MaxInfo* leader = NULL;
  value_t k = 1, total = ROWSIZE*COLSIZE;
  for(int32_t i=0; i < COLSIZE; ++i){
    for(int32_t j=0; j < ROWSIZE; ++j){
      cout << "Processing (" << (100.0*k)/total << "%)       \r";
      table[i][j].val = lagfib(k, small, big);
      if(k == 10)
	assert(table[i][j].val == -393027);
      else if(k == 100)
	assert(table[i][j].val == 86613);
      process(table, i, j);
      if(leader == NULL || table[i][j].maxval > leader->maxval){
	leader = &table[i][j];
      }
      ++k;
    }
  }
  cout<<endl<<"The answer is: "<<leader->maxval<<endl;
  return 0;
}
