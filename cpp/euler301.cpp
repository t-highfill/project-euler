
#include <pthread.h>
#include <iostream>
#include <cstdint>
#include <cassert>
#include <cstdlib>

#define nimsum(a,b,c) ((a)^(b)^(c))
#define MIN(a,b) ((a) < (b) ? (a) : (b))

#define THREADS 10

typedef uint32_t nim_t;

typedef struct count_mutex{
  nim_t count;
  pthread_mutex_t* mutex;
 } count_mutex;

typedef struct thread_info{
  count_mutex* c_mut;
  nim_t start, stop;
} thread_info;

/*
nim_t nimsum(nim_t a, nim_t b, nim_t c){
  return a^b^c;
}

double percent(double n, double max){
  return 100*n/max;
}
*/

void* thread_run(void* data){
  thread_info* info = (thread_info*) data;
  count_mutex* c_mut = info->c_mut;
  nim_t count = 0;
  for(nim_t n=info->start; n < info->stop; ++n){
    if(nimsum(n, 2*n, 3*n) == 0){
      ++count;
    }
  }
  pthread_mutex_lock(c_mut->mutex);
  c_mut->count += count;
  pthread_mutex_unlock(c_mut->mutex);
  return NULL;
}

int main(){
  const nim_t max = 1 << 30;
  pthread_mutex_t mutex;
  count_mutex c_mut;
  c_mut.count = 0;
  c_mut.mutex = &mutex;
  pthread_mutex_init(c_mut.mutex, NULL);

  thread_info infos[THREADS];
  pthread_t threads[THREADS];

  nim_t chunk_size = max / THREADS;
  assert(nimsum(1,2,3) == 0);
  for(nim_t n = 0; n < THREADS; ++n){
    infos[n].c_mut = &c_mut;
    infos[n].start = n*chunk_size;
    infos[n].stop = MIN(max+1, (n+1)*chunk_size);
    if(pthread_create(threads+n, NULL, thread_run, infos+n) != 0){
      perror("pthread_create");
      return -1;
    }
  }
  for(nim_t n = 0; n < THREADS; ++n){
    if(pthread_join(threads[n], NULL) != 0){
      perror("pthread_join");
      return -1;
    }
  }
  std::cout << "\nDone! count = " << c_mut.count << std::endl;
  return 0;
}
