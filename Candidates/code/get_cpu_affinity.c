#define _GNU_SOURCE

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <sched.h>

/* Inspired by code from util-linux-2.13-pre7/schedutils/taskset.c */
// get cpu affinity bit mask and string representation
// strsize = size of str
// returns number of characters in str
int GetCpuAffinity(cpu_set_t *mask, char *str, int strsize)
{
  char *ptr = str;
  int i, j, nc, entry_made = 0;
  size_t freesize = strsize;

  (void)sched_getaffinity(0, sizeof(*mask), mask);
  *ptr = '\0';
  for (i = 0; i < CPU_SETSIZE; i++) {
    if (CPU_ISSET(i, mask)) {
      int run = 0;
      entry_made = 1;
      for (j = i + 1; j < CPU_SETSIZE; j++) {
        if (CPU_ISSET(j, mask)) run++;
        else                    break;
      }
      nc = 0;
      if (!run)
        nc = snprintf(ptr, freesize, "%d,", i);             // one hwthread
      else if (run == 1) {
        nc = snprintf(ptr, freesize, "%d,%d,", i, i + 1);   // two consecutive hwthreads
        i++;
      } else {
        nc = snprintf(ptr, freesize, "%d-%d,", i, i + run); // 3 or more consecutive hwthreads
        i += run;
      }
      freesize -= nc;
      ptr += nc;
    }
  }
  ptr -= entry_made;
  *ptr = 0;
  return(ptr - str); // number of characters in string
}
