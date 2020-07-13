
#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <limits.h>

#define SIZEOF(a) (sizeof(a) == 4) ? "C_INT" : "C_LONG_LONG" 

int main(){
  printf("integer, parameter :: C_SSIZE_T = %s\n",SIZEOF(ssize_t));
  printf("integer, parameter :: C_TIME_T = %s\n",SIZEOF(time_t));
  printf("integer, parameter :: C_SUSECONDS_T = %s\n",SIZEOF(suseconds_t));
  printf("integer, parameter :: C_USECONDS_T = %s\n",SIZEOF(useconds_t));
  printf("integer, parameter :: C_CLOCK_T = %s\n",SIZEOF(clock_t));
  printf("real,    parameter :: C_CLOCKS_PER_SEC = %ld.0\n",CLOCKS_PER_SEC);
  printf("integer, parameter :: C_MODE_T = %s\n",SIZEOF(mode_t));
  printf("integer, parameter :: C_OFF_T  = %s\n",SIZEOF(off_t));
  printf("integer, parameter :: O_APPEND = %d\n",O_APPEND);
  printf("integer, parameter :: O_CREAT  = %d\n",O_CREAT);
  printf("integer, parameter :: O_EXCL   = %d\n",O_EXCL);
  printf("integer, parameter :: O_TRUNC  = %d\n",O_TRUNC);
  printf("integer, parameter :: O_RDWR   = %d\n",O_RDWR);
  printf("integer, parameter :: O_RDONLY = %d\n",O_RDONLY);
  printf("integer, parameter :: O_WRONLY = %d\n",O_WRONLY);
  printf("integer, parameter :: SEEK_SET = %d\n",SEEK_SET);
  printf("integer, parameter :: SEEK_CUR = %d\n",SEEK_CUR);
  printf("integer, parameter :: SEEK_END = %d\n",SEEK_END);
  printf("integer, parameter :: PATH_MAX = %d\n",PATH_MAX);
  return 0;
}
