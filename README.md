# Zelkel

## Todo
- way to implement multi type stack 
```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    void* array[3];

    int a = 10;
    float b = 5.5;
    char c = 'X';

    array[0] = &a;
    array[1] = &b;
    array[2] = &c;

    printf("Element 0: %d\n", *(int*)array[0]);
    printf("Element 1: %.2f\n", *(float*)array[1]);
    printf("Element 2: %c\n", *(char*)array[2]);

    return 0;
}
```
- [ ] Rewrite in itself
- [ ] Remove dependence on c libraies: stdlib.h and stdio.h
- [ ] Implement more data types: strings, floats. Upgrade the stack to store pointers and then translate them to correct type