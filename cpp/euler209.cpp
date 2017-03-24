
#include <iostream>
#include <cstdint>
#include <array>

#define A_MASK 0b100000
#define B_MASK 0b010000
#define C_MASK 0b001000

#define CHECK_MASK(n, mask) ((n & mask) != 0 ? 1 : 0)

#define SET_BIT_ZERO(n, bit) (n & (~(1<<bit)))
#define SET_BIT_ONE(n, bit) (n | (1<<bit))

#define BITLENGTH 64

typedef uint64_t column_t;
typedef uint8_t row_t;
typedef std::array<bool,BITLENGTH> bitset_t;

row_t transform(row_t n){
  row_t a, b, c;
  a = CHECK_MASK(n, A_MASK);
  b = CHECK_MASK(n, B_MASK);
  c = CHECK_MASK(n, C_MASK);
  return ((n << 1) | (a^(b & c))) % BITLENGTH;
}

uint64_t countNums(column_t& col, bitset_t& set, row_t n=0){
  uint64_t result = 0;
  //Find first bit not set
  while(set[n]){
    ++n;
    if(n >= BITLENGTH){//Left the space
      return 1;
    }
  }
  set[n] = true;
  row_t t_n = transform(n);
  if(set[t_n]){//The transformed bit is set
    row_t t_bit = col & (1<<t_n);
    if(t_bit != 0){//The bit is a 1
      col = SET_BIT_ZERO(col, n);//This bit must be 0
    }else{         //The bit is a 0
      bitset_t tmpset = set;
      col = SET_BIT_ZERO(col,n);
      result += countNums(col, tmpset, n);
      tmpset = set;
      col = SET_BIT_ONE(col,n);
      result += countNums(col, tmpset, n);
    }
  }else{       //The transformed bit is not set
    set[t_n] = true;
    bitset_t tmpset = set;
    {//Set this bit to one
      col = SET_BIT_ONE(col, n);
      col = SET_BIT_ZERO(col, t_n);//This one has to be 0
      result += countNums(col, tmpset, n);
    }
    {//Set this bit to zero
      col = SET_BIT_ZERO(col, n);
      //Set other to zero
      col = SET_BIT_ZERO(col, t_n);
      result += countNums(col, tmpset, n);
      //Set other to one
      col = SET_BIT_ONE(col, t_n);
      result += countNums(col, tmpset, n);
    }
  }
  return result;
}

int main(){
  bitset_t set;
  set.fill(false);
  column_t col = 0;
  std::cout << "Result: " << countNums(col, set) << std::endl;
  return 0;
}
