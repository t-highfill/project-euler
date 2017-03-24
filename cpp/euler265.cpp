
#include <iostream>
#include <array>
#include <bitset>

#define USE_HEAP 0
#define CONST_N 5
#define TWO_TO_THE_N (1<<CONST_N)

typedef std::bitset<CONST_N> subseq_t;
typedef std::bitset<TWO_TO_THE_N> seq_t;

typedef std::array<bool, TWO_TO_THE_N> seq_set_t;

template<size_t size>
bool all(const std::array<bool, size>& arr){
  for(size_t i=0; i<size; ++i){
    if(!arr[i])
      return false;
  }
  return true;
}

template<size_t size>
void leftShift(std::bitset<size>& bset, size_t shift = 1){
  //std::cout << "Left shifting " << bset << " by " << shift;
  //std::cout << " (size is " << size << ")" << std::endl;
  size_t i;
  for(i=size-1; i>=shift; --i){
    bset.set(i, bset[i-shift]);
  }
  for(; i>=0; --i){
    //std::cout << i << std::endl;
    bset.set(i,false);
    if(i == 0)
      break;
  }
}

bool checkSeq(seq_t& seq){
  for(size_t i = 1; i<=CONST_N; ++i){
    if(seq[seq.size()-i])
      return false;
  }
  return true;
}

unsigned long addSeqs(seq_t& seq, subseq_t& last, seq_set_t& seqSet,
		      seq_t& tracker, size_t active);
#if USE_HEAP
template<class T>
T& heapCopy(const T& orig){
  return *(new T(orig));
}

unsigned long addSeqs_(seq_t& seq, subseq_t& last, seq_set_t& seqSet,
		       seq_t& tracker, size_t active){
  //seq = *(new seq_t(seq));
  return addSeqs(heapCopy(seq), heapCopy(last), heapCopy(seqSet),
		 heapCopy(tracker), active);
}
#else
unsigned long addSeqs_(seq_t seq, subseq_t last, seq_set_t seqSet,
		       seq_t tracker, size_t active){
  return addSeqs(seq, last, seqSet, tracker, active);
}
#endif //USE_HEAP

unsigned long addSeqs(seq_t& seq, subseq_t& last, seq_set_t& seqSet,
		      seq_t& tracker, size_t active){
  seqSet[last.to_ulong()] = true;
  if(tracker[tracker.size()-1]){
    if(all(seqSet)){
      if(checkSeq(seq)){
	std::cout << "Found sequence: " << seq << std::endl;
	return seq.to_ulong();
      }else{
	return 0;
      }
    }
    if(active == 0){
      active = seq.size()-1;
    }else{
      --active;
    }
  }else{
    leftShift(tracker);
    leftShift(seq);
  }
  leftShift(last);
  subseq_t other(last);
  other.set(0, true);
  if(seqSet[last.to_ulong()]){
    seq.set(active, true);
    return addSeqs_(seq, other, seqSet, tracker, active);
  }else if(seqSet[other.to_ulong()]){
    seq.set(active, false);
    return addSeqs_(seq, last, seqSet, tracker, active);
  }else{
    seq.set(active, false);
    unsigned long res = addSeqs_(seq, last, seqSet, tracker, active);
    seq.set(active, true);
    return res + addSeqs_(seq, other, seqSet, tracker, active);
  }
}

int main(){
  seq_t seq;
  seq_t tracker;
  subseq_t sub;
  seq_set_t seqSet;
  seqSet.fill(false);
  tracker.set(CONST_N-1);
  std::cout << "N = " << CONST_N << "\t2^N = " << TWO_TO_THE_N << std::endl;
  std::cout << "Result: " << addSeqs_(seq, sub, seqSet, tracker, 0) << std::endl;
  return 0;
}
