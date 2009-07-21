#include <iostream>
#include "Formula.h"

using namespace Formula;

template <class Num>
Num factorial( Num of )
{
    if ( of <= 1 )
        return of;

    return of * factorial<Num>( of - 1 );
}

template <class Num>
Num testFromNum()
{
    Num    num = 2;
    return (1 + num) * (2 - num) /
           (3 * num) + (4 / num) - 
           (5 % num) % -num;
}

template <class Num>
Num testFromNum2()
{
    Num    num = 2.0;
    return (1.0 + num) * (2.0 - num) /
           (3.0 * num) + (4.0 / num) - 0.0;
}

int main(int argc, char *argv[])
{
    FormulaTracedNumber<int>    num( "x", 5 );
    FormulaTracedNumber<int>    num2 = factorial( num );
    
    num2.getFormula()->show( std::cout );
    std::cout << std::endl;

    // HAHA more test
    FormulaTracedNumber<double> num3( 15.0f );
    FormulaTracedNumber<double> num4 =
        sin( cos( exp( - (++num3) ) ) ) / 2.0f;

    num4.getFormula()->show( std::cout );
    std::cout << std::endl;

    // lalalalala
    FormulaTracedNumber<int> num5 = testFromNum<FormulaTracedNumber<int> >();
    num5.getFormula()->show( std::cout );
    std::cout << std::endl;

    FormulaTracedNumber<double> num6 = testFromNum2<FormulaTracedNumber<double> >();
    num6.getFormula()->show( std::cout );
    std::cout << std::endl;

    int dada = 0;
    dada += num5;

    return 0;
}

