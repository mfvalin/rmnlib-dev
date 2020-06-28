
#include <stdio.h>
#include <time.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>

int main(){
  printf("integer, parameter :: C_SSIZE_T = %ld\n",sizeof(ssize_t));
  printf("integer, parameter :: C_TIME_T = %ld\n",sizeof(time_t));
  printf("integer, parameter :: C_SUSECONDS_T = %ld\n",sizeof(suseconds_t));
  printf("integer, parameter :: C_CLOCK_T = %ld\n",sizeof(clock_t));
  printf("integer, parameter :: C_CLOCKS_PER_SEC = %ld\n",CLOCKS_PER_SEC);
  printf("integer, parameter :: C_MODE_T = %ld\n",sizeof(mode_t));
  printf("integer, parameter :: O_APPEND = %d\n",O_APPEND);
  printf("integer, parameter :: O_CREAT  = %d\n",O_CREAT);
  printf("integer, parameter :: O_EXCL   = %d\n",O_EXCL);
  printf("integer, parameter :: O_TRUNC  = %d\n",O_TRUNC);
  printf("integer, parameter :: O_RDWR   = %d\n",O_RDWR);
  printf("integer, parameter :: O_RDONLY = %d\n",O_RDONLY);
  printf("integer, parameter :: O_WRONLY = %d\n",O_WRONLY);
  return 0;
}
