
#include <iostream>
#include <cstdint>
#include <cstdlib>
#include <pthread.h>
#include <string.h>

#define NUM_THREADS 4
#define TRY_SYSCALL(call) {int e=(call);if(e != 0){\
      std::cerr<<#call<<": "<<strerror(e)<<std::endl;exit(-1);}}
#define DISP(x) {std::cout<<#x<<" = "<<(x)<<std::endl;}

typedef uint64_t idx_t;
typedef uint64_t sum_t;

struct threaded_sum{
  pthread_mutex_t mutex;
  sum_t sum;
};

struct thread_data{
  threaded_sum* sum_ptr;
  idx_t start, stop;
};

sum_t idempotent(sum_t n){
  if(n == 1) return 0;
  sum_t a;
  for(a=n-1; (a*a)%n!=a; --a){}
  return a;
}

void* runThread(void* data){
  thread_data* tdata = (thread_data*) data;
  sum_t p_sum = 0;
  //DISP(tdata->start);
  //DISP(tdata->stop);
  for(idx_t i=tdata->start; i <= tdata->stop; ++i){
    //DISP(i);
    p_sum += idempotent((sum_t) i);
  }
  TRY_SYSCALL(pthread_mutex_lock(&tdata->sum_ptr->mutex));
  tdata->sum_ptr->sum += p_sum;
  TRY_SYSCALL(pthread_mutex_unlock(&tdata->sum_ptr->mutex));
  return NULL;
}

int main(){
  DISP(idempotent(1));
  DISP(idempotent(6));
  threaded_sum result;
  TRY_SYSCALL(pthread_mutex_init(&result.mutex, NULL));
  pthread_t threads[NUM_THREADS];
  thread_data datas[NUM_THREADS];
  idx_t step = 10000000 / NUM_THREADS;
  for(idx_t i=0; i<NUM_THREADS; i++){
    datas[i].sum_ptr = &result;
    datas[i].start = i*step+1;
    datas[i].stop  = (i+1)*step;
    std::cout << "start = " << datas[i].start
	      << "\tstop = " << datas[i].stop << std::endl;
    TRY_SYSCALL(pthread_create(&threads[i], NULL, runThread, datas+i));
  }
  for(int i=0; i<NUM_THREADS; ++i){
    TRY_SYSCALL(pthread_join(threads[i], NULL));
    std::cout << "Thread " << i << " exited!" << std::endl;
  }
  std::cout << "Result: " << result.sum << std::endl;
  return 0;
}
