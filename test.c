#include <stdio.h>
typedef struct a
{
    int x;
    float y;
} pos;
// some comment
/* more comment ***********/
int main()
{
    int h = 0x1f;
    int o = 0; /* more
    more
    comment *******
    ****/

    double f = 5.;
    double g = .5;
    pos m5;
    char a = 'a\
\
\
\
\
\
\
';
    m5.x = 1;
    m5.y = .5;
    printf("%d\
,%f",
           m5.x, m5.y);
    return 0;
}