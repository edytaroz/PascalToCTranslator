#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
int num1;
int num2;
int num3;
int num4;
int num5;
int k;
int main(){
num1 = 5;
num2 = 20000;
num3 = 2;
num4 = 1;
num5 = 0;
while(num1 <= num2){
num5 = num5+1;
if(num5 > 1){
printf(num5);
}
printf(num1);
if(num5 == num4 and !(num1*2 > num2)){
printf(num4);
num5 = 0;
}
num1 = num1*num3;
}
for(k=0;k<2;k++){
printf('Hello');
}
for(k=4;k>0;k--){
printf('World');
}
printf(num1);
return 0;
}

