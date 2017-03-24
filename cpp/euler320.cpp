
#include <iostream>
#include <cstdint>
#include <pthread.h>

template<class T>
T powMod(T mod, T base, T exp){
	switch(exp){
		case 0: return 1;
		case 1: return base % mod;
		default:
		if(exp % 2 == 0){
			T tmp = powMod(mod, base, exp/2);
			return (tmp*tmp) % mod;
		}else{
			return ((base % mod)*powMod(mod, base, exp-1)) % mod;
		}
	}
}

