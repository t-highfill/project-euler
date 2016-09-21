#include <iostream>

using namespace std;

int countDistinctFactors(int n) {
  int c = 0; 
  for(int p=2; n>=1; p++) { 
    if(n % p == 0) { 
      c++; 
      while(n % p == 0) 
	n /= p;
    }
  }
  return c; 
}

int main() { 
  int counter = 0; 
  for(int n=2; ; n++) { 
    if(countDistinctFactors(n)==4) 
      counter++; 
    else 
      counter=0; 
    if(counter==4) { 
      cout << n-3 << endl; 
      break; 
    } 
  } 
  return 0; 
}
