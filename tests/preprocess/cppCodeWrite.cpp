#include <iostream>

using namespace std;

int main(int argc, char *argv[])
{
    double x = 3, y = 15;
    double z;
    // Eq:eval z := derivate( sqrt( x * x^3 + y * y ), x )
    //<@<
    z = (3 * x * pow( x, 2 )  + pow( x, 3 ) ) / (2 * sqrt(x * pow( x, 3 )  + y * y));

    //>@>

    // Eq:eval w := sum( i = 0, 5, x * i + 1 )
    //<@<
    w = 1 + (1 + x) + (1 + 2 * x) + (1 + 3 * x) + (1 + 4 * x) + (1 + 5 * x);

    //>@>
    return 0;
}
