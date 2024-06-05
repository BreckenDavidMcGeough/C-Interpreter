int x=5+3*2; //x=11
int y=4*x+3; //y=47
int z=x+y*4-2; //z=197
z=1; //z=1
z=2+3*y; //z=143
int b=4; //b=4
x=3+(p=2); //x=5
int m=t=5; //m=5
m=e=x; //m=5
y=10; //y=10
x=5+(y=9); //x=14
y=1; //y=1
y=1+y; //y=2
x=y+1; //x=3
x=x+3+x; //x=9






int i = 3;
i = i + 7*(i = 4); //31
int j = 3;
j = j + 7*(j = 4); //31
j = (i = 5) + i + (i = 10) + i; //30



int x = y+6;
























//hw6test.c

#include <stdio.h>

int main(){

	int i = 3; 
	i = i + 7*(i=4); //i=31
	int j = 3;
	j = j + 7*(j=4); //j=31
	j=(i=5)+i+0*(i=10)+i; //j=20

	return 0;

}




















//cfile.c

#include <stdio.h>

int main(){

	int i = 4;  
	int k = 5*(4+i)*i+5/5;//k=161
	i = (i=3) + i * 4; //i=15
	int k = i + 3*(4+i); // 72

	return 0;
}
