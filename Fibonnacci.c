#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
int fibonacci(int n){
int fibonacci;
if(n > 1){
fibonacci = fibonacci(n-2)+fibonacci(n-1);
}
else{
fibonacci = n;
}
return fibonacci;
}
int main(){
printf(fibonnacci(5));
return 0;
}

