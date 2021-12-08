#define _GNU_SOURCE

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <sched.h>
#include <mpi.h>
#include <omp.h>
#include <sys/shm.h>
#include <sys/stat.h>
#include <fcntl.h>

static int id;
static struct shmid_ds shm_buf;
static void *ptr;
size_t size=1024*1024;

int get_cpu_hyperthreads(){
  int fd=open("/sys/devices/system/cpu/smt/active",O_RDONLY);
  char ht = '0';
  read(fd,&ht,1l);
  close(fd);
  if(ht == '0') return 1;
  return 2;
}

/* Borrowed from util-linux-2.13-pre7/schedutils/taskset.c */
static char *cpuset_to_cstr(cpu_set_t *mask, char *str)
{
  char *ptr = str;
  int i, j, entry_made = 0;
  for (i = 0; i < CPU_SETSIZE; i++) {
    if (CPU_ISSET(i, mask)) {
      int run = 0;
      entry_made = 1;
      for (j = i + 1; j < CPU_SETSIZE; j++) {
        if (CPU_ISSET(j, mask)) run++;
        else break;
      }
      if (!run)
        sprintf(ptr, "%d,", i);
      else if (run == 1) {
        sprintf(ptr, "%d,%d,", i, i + 1);
        i++;
      } else {
        sprintf(ptr, "%d-%d,", i, i + run);
        i += run;
      }
      while (*ptr != 0) ptr++;
    }
  }
  ptr -= entry_made;
  *ptr = 0;
  return(str);
}

static cpu_set_t set;
static int lo_core=-1;
static int hi_core=-1;

void RebindBySocket(int init) {
  int rank, rank0, size, ncores, i, npersock;
  char *omp;
  MPI_Comm comm;
  int color;
  int get_cpu_cores();
  int get_cpu_hyperthreads();
  int *flag;

if(init){
  omp=getenv("OMP_NUM_THREADS");
  color=gethostid();
  color &= 0x7FFFFFFF;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank0);
  MPI_Comm_split(MPI_COMM_WORLD,color,rank0,&comm);
  MPI_Comm_size(comm, &size);
  MPI_Comm_rank(comm, &rank);

  ncores=sysconf(_SC_NPROCESSORS_CONF);
  ncores /= get_cpu_hyperthreads();
  npersock=ncores/2;
  CPU_ZERO(&set);
  if(rank < size/2) {
    lo_core = 0 ;
    hi_core = npersock - 1 ;
  }else{
    lo_core = npersock ;
    hi_core = ncores - 1;
  }
for(i=lo_core ; i < hi_core ;  i++) { CPU_SET(i,&set) ;}
}
  sched_setaffinity(0,sizeof(set),&set);
}

void RebindMyCores(int init) {
  int rank, rank0, size, ncores, i, nthreads, numa;
  int t[128];
  char *omp;
  MPI_Comm comm;
  int color;
  int get_cpu_cores();
  int get_cpu_hyperthreads();
  int *flag;

if(init){
  omp=getenv("OMP_NUM_THREADS");
  color=gethostid();
  color &= 0x7FFFFFFF;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank0);
  MPI_Comm_split(MPI_COMM_WORLD,color,rank0,&comm);
  MPI_Comm_size(comm, &size);
  MPI_Comm_rank(comm, &rank);

  if(rank == 0) {
    id=shmget(IPC_PRIVATE,size,IPC_CREAT|S_IRUSR|S_IWUSR);  /* rank 0 allocates shared memory segment */
    ptr=shmat(id,NULL,0);                 /* attach rank 0 to segment */
    shmctl(id,IPC_RMID,&shm_buf);         /* mark segment for removal */
  }
  MPI_Bcast(&id,1,MPI_INTEGER,0,comm);    /* broadcast segment id */
  if(rank != 0) ptr=shmat(id,NULL,0);     /* attach other ranks to segment */
  flag = (int *) ptr;
  flag[rank] = rank;
  MPI_Barrier(comm);
  if(rank == 0) {
    if(flag[1] == 1) printf("test successful on rank %d\n",rank);
  }else{
    if(flag[rank-1] == rank-1)  printf("shared memory test successful on rank %d\n",rank);
  }

  nthreads = 1;
  if(omp != NULL) nthreads=atoi(omp);
  ncores=sysconf(_SC_NPROCESSORS_CONF);
  ncores /= get_cpu_hyperthreads();
//  numa = get_cpu_cores();
numa=ncores/2;
  MPI_Allgather(&nthreads,1,MPI_INTEGER,&t[1],1,MPI_INTEGER,comm);
  t[0] = 0;
  for(i=1;i<=size;i++) {
    t[i] = t[i-1] + t[i];
    if(t[i] < numa && t[i] + t[i+1] > numa) t[i]=numa;
    if(t[i] > ncores) t[i] = ncores;
  }
  CPU_ZERO(&set);
if(rank < size/2) {
  lo_core = 0 ;
  hi_core = numa - 1 ;
}else{
  lo_core = numa ;
  hi_core = ncores - 1;
}
for(i=lo_core ; i < hi_core ;  i++) { CPU_SET(i,&set) ;}
//  for(i=t[rank] ; i < t[rank+1] ; i++) { CPU_SET(i,&set) ;}
//  lo_core = t[rank];
//  hi_core = t[rank+1]-1;
}
  sched_setaffinity(0,sizeof(set),&set);
#if defined(NEVER_TRUE)
  #pragma omp parallel shared(set)
  { 
    sched_setaffinity(0,sizeof(set),&set); 
  }
#endif
}

int main(int argc, char *argv[])
{
  int thread, size0, rank0;
  cpu_set_t coremask;
  char clbuf[7 * CPU_SETSIZE], hnbuf[64];
  int flag, statut;
  int delay=1;

  statut = MPI_Initialized(&flag);
  if(! flag) MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank0);
  MPI_Comm_size(MPI_COMM_WORLD, &size0);
  memset(hnbuf, 0, sizeof(hnbuf));
  (void)gethostname(hnbuf, sizeof(hnbuf));
  memset(clbuf, 0, sizeof(clbuf));

  #pragma omp parallel private(thread, coremask, clbuf)
  {
    thread = omp_get_thread_num();
    (void)sched_getaffinity(0, sizeof(coremask), &coremask);
    cpuset_to_cstr(&coremask, clbuf);
    #pragma omp barrier
if(argc > 1)
    printf("%s Before: Hello from rank %3.3d of %3.3d, thread %3.3d, on %s. EVAR=%s (core affinity = %s)\n",
            argv[0],rank0, size0, thread, hnbuf, getenv("EVAR"), clbuf);
  }

  memset(clbuf, 0, sizeof(clbuf));
  RebindBySocket(1);
  snprintf(clbuf,sizeof(clbuf),"%d-%d",lo_core,hi_core);
  if(getenv("DELAY") != NULL) delay = atoi(getenv("DELAY"));
  sleep(delay);
#if 1
  #pragma omp parallel private(thread, coremask, clbuf)
  {
    RebindBySocket(0);
    thread = omp_get_thread_num();
    (void)sched_getaffinity(0, sizeof(coremask), &coremask);
    cpuset_to_cstr(&coremask, clbuf);
    #pragma omp barrier
if(thread == 0)
    printf("%s after : Hello from rank %3.3d of %3.3d, thread %3.3d, on %s. EVAR=%s (core affinity = %s)\n",
            argv[0],rank0, size0, thread, hnbuf, getenv("EVAR"), clbuf);
  }
#endif
  MPI_Finalize();
  return(0);
}
