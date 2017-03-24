
#include <cstdint>
#include <iostream>
#include <map>

typedef uint32_t denom_t;
typedef std::map<denom_t, bool> known_map;

bool validate(denom_t d, known_map& known){
	while(d >= 111){
		// auto itr = known.find(d);
		// if(itr != known.end())
		// 	return itr->second;
		if((d % 1000) % 111 == 0){
			return false;
		}
		d /= 10;
	}
	return true;
}

int main(){
	known_map *known = new known_map();
	denom_t d = 1, max=-1;
	double res = 0;
	std::cout.precision(10);
	std::cout.setf( std::ios::fixed, std:: ios::floatfield ); // floatfield set to fixed
	std::cout << "Starting..." << std::endl;
	while(d < max){
		if(validate(res, *known)){
			res += 1.0 / d;
			std::cout << "res = " << res << "\td = " << d << '\r' << std::flush;
		}
		++d;
	}
	delete known;
}