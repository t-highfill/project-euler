
#ifndef __BITWISESET__
#define __BITWISESET__

#include <cstdint>
#include <cstdlib>

template<class T>
class bitwiseset{
private:
  T arr_len;
  uint64_t* arr;
  const uint8_t bit_len = 64;
  bool def;

  void grow(T newsize){
    if(newsize <= arr_len)
      return;
    uint64_t* tmp = new uint64_t[newsize];
    T i=0;
    for(;i < arr_len; ++i){
      tmp[i] = arr[i];
    }
    for(;i < newsize; ++i){
      tmp[i] = def ? UINT64_MAX : 0;
    }
    delete[] arr;
    arr = tmp;
  }
  void check_grow(T size){
    if(size >= arr_len){
      grow(arr_len*2);
    }
  }
public:
  bitwiseset(T size=50, bool def=false){
    arr_len = size/bit_len + 1;
    arr = new uint64_t[arr_len];
    for(T i=0; i<arr_len; ++i){
      arr[i] = def ? UINT64_MAX : 0;
    }
    this->def = def;
  }
  ~bitwiseset(){
    delete[] arr;
  }
  bool get(T key) const{
    T idx = key/bit_len;
    uint64_t mask = 1 << (key % bit_len);
    if(idx >= arr_len){
      return false;
    }
    return (arr[idx] & mask) != 0;
  }
  bool get(T key){
    T idx = key/bit_len;
    uint64_t mask = 1 << (key % bit_len);
    check_grow(idx+1);
    return (arr[idx] & mask) != 0;
  }
  bool set(T key, bool val){
    T idx = key/bit_len;
    uint64_t mask = 1 << (key % bit_len);
    check_grow(idx+1);
    if(val){
      arr[idx] = arr[idx] | mask;
    }else{
      arr[idx] = arr[idx] & (~mask);
    }
    return val;
  }
  
};

#endif //__BITWISESET__
