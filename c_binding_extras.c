
#include <stdio.h>
#include <time.h>
#include <sys/time.h>

int main(){
  printf("integer, parameter :: C_TIME_T = %ld\n",sizeof(time_t));
  printf("integer, parameter :: C_SUSECONDS_T = %ld\n",sizeof(suseconds_t));
  printf("integer, parameter :: C_CLOCK_T = %ld\n",sizeof(clock_t));
  printf("integer, parameter :: C_CLOCKS_PER_SEC = %ld\n",CLOCKS_PER_SEC);
  return 0;
}
