#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
int fibonacci(int n){
int fibonacci1;
if(n > 1){
fibonacci1 = fibonacci(n-2)+fibonacci(n-1);
}
else{
fibonacci1 = n;
}
return fibonacci1;
}
int main(){
printf(fibonacci(5));
return 0;
}

