#include <sys/time.h>
#include <sys/resource.h>
#include <stdint.h>

typedef struct {
  int64_t usr_cpu ;
  int64_t sys_cpu ;
  int64_t max_rss ;
}f_rusage ;

int32_t F90_getrusage(int who, f_rusage *usage){
  struct rusage t ;
  int status ;

  status = getrusage(who, &t) ;

  usage->usr_cpu  = t.ru_utime.tv_sec ;
  usage->usr_cpu *= 1000000 ;
  usage->usr_cpu += t.ru_utime.tv_usec ;

  usage->sys_cpu  = t.ru_stime.tv_sec ;
  usage->sys_cpu *= 1000000 ;
  usage->sys_cpu += t.ru_stime.tv_usec ;

  usage->max_rss = t.ru_maxrss ;
}
