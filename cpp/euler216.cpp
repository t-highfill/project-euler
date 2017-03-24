
#include <iostream>
#include <cstdint>
#include <cmath>
#include "bitwiseset.cpp"

typedef uint64_t prime_t;
typedef bitwiseset<prime_t> pmap_t;

void sieve(pmap_t& pmap, prime_t limit){
  prime_t end = floor(sqrt(limit));
  for(prime_t i=2; i<=end; ++i){
    if(pmap.get(i)){
      for(prime_t j=i*i; j <= limit; j += i){
	pmap.set(j,false);
      }
    }
  }
}

prime_t lilT(prime_t n){
  return 2*n*n-1;
}

prime_t result(prime_t limit){
  prime_t ceiling = lilT(limit)+1;
  pmap_t pmap(ceiling,true);
  std::cout << "Generating primes..." << std::flush;
  sieve(pmap, ceiling);
  std::cout << "Done!" << std::endl;
  prime_t res = 0;
  for(prime_t n=2; n <= limit; ++n){
    prime_t p = lilT(n);
    if(pmap.get(p)){
      std::cout << p << " " << std::flush;
      ++res;
      //std::cout << "Counting: " << res << '\r' << std::flush;
    }
  }
  std::cout << "Counting: DONE!" << std::endl;
  return res;
}

int main(){
  //prime_t res = result(50*1000*1000);
  prime_t res = result(10000);
  std::cout << "Result :" << res << std::endl;
  return 0;
}
