#include <iostream>
#include "Par.h"
using namespace std;

// Propósito: construye un par
Par consPar(int x, int y)
{
    Par p = {x, y};
    return p;
}

// Propósito: devuelve la primera componente
int fst(Par p)
{
    return p.x;
}

// Propósito: devuelve la segunda componente
int snd(Par p)
{
    return p.y;
}

// Propósito: devuelve la mayor componente
int maxDelPar(Par p)
{
    return p.x > p.y ? p.x : p.y;
}

// Propósito: devuelve un par con las componentes intercambiadas
Par swap(Par p)
{
    Par p2 = {p.y, p.x};
    return p2;
}

// Propósito: devuelve un par donde la primer componente
// es la división y la segunda el resto entre ambos números
Par divisionYResto(int n, int m)
{
    Par p = {n / m, n % m};
    return p;
}