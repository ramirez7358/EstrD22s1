#include <iostream>
#include <cassert>
#include "Par.cpp"
using namespace std;

int main()
{
    Par p = consPar(10, 2);
    Par p2 = consPar(12, 5);

    // cons par test
    assert(p.x == 10);
    assert(p.y == 2);
    assert(p2.x == 12);
    assert(p2.y == 5);

    // fst test
    assert(fst(p) == 10);
    assert(fst(p2) == 12);

    // snd test
    assert(snd(p) == 2);
    assert(snd(p2) == 5);

    // max del par test
    assert(maxDelPar(p) == 10);
    assert(maxDelPar(p2) == 12);

    // swap test
    Par pSwaped = swap(p);
    assert(pSwaped.x == 2);
    assert(pSwaped.y == 10);

    Par p2Swaped = swap(p2);
    assert(p2Swaped.x == 5);
    assert(p2Swaped.y == 12);

    // divisionYResto test

    Par pdyr = divisionYResto(10, 10);
    assert(pdyr.x == 1);
    assert(pdyr.y == 0);
}