#include <iostream>
#include "ArrayList.h"
using namespace std;

struct ArrayListSt
{
    int cantidad;   // cantidad de elementos
    int *elementos; // array de elementos
    int capacidad;  // tamaño del array
};

ArrayList newArrayListWith(int capacidad)
{
    ArrayListSt *array = new ArrayListSt;
    array->cantidad = 0;
    array->elementos = new int[capacidad];
    array->capacidad = capacidad;

    return array;
}

ArrayList newArrayList()
{
    return newArrayListWith(16);
};

int lengthAL(ArrayList xs)
{
    return xs->cantidad;
}

int get(int i, ArrayList xs)
{
    return xs->elementos[i];
}

void set(int i, int x, ArrayList xs)
{
    xs->elementos[i] = x;
}

void resize(int capacidad, ArrayList xs)
{
    if (capacidad < xs->capacidad)
    {
        int *nuevosElementos = new int[capacidad];
        memcpy(nuevosElementos, xs->elementos, capacidad);
        xs->elementos = nuevosElementos;
    }
    xs->cantidad = capacidad;
    xs->capacidad = capacidad;
}

void add(int x, ArrayList xs)
{
    xs->cantidad += 1;
    xs->elementos[xs->cantidad - 1] = x;
}

void remove(ArrayList xs)
{
    int *nuevosElementos = new int[xs->capacidad - 1];
    memcpy(nuevosElementos, xs->elementos, xs->capacidad - 1);
    xs->elementos = nuevosElementos;
    xs->cantidad -= 1;
}