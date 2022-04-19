#include <stdio.h>

int Demo1(int a)
{
     printf("Demo1 : a = %d\n",a);
    return(a);
}
int Demo1b(int *a)
{
     printf("Demo1b : a = %d\n",*a);
    return(*a);
}
float Demo2(int a, float b)
{
     printf("Demo2 : a = %d, b = %f\n",a, b);
    return (b+a);
}
float Demo2b(int *a, float *b)
{
     printf("Demo2b : a = %d, b = %f\n",*a, *b);
    return (*b+*a);
}
