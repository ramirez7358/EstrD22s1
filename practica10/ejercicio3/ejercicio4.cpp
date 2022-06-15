#include <iostream>
#include "ArrayList.cpp"
using namespace std;

// Devuelve la suma de todos los elementos.
int sumatoria(ArrayList xs)
{
    int counter = 0;
    for (int i = 0; i < lengthAL(xs); i++)
    {
        counter += get(i, xs);
    }

    return counter;
}

// Incrementa en uno todos los elementos.
void sucesores(ArrayList xs)
{
    for (int i = 0; i < lengthAL(xs); i++)
    {
        set(i, get(i, xs) + 1, xs);
    }
}

// Indica si el elemento pertenece a la lista.
bool pertenece(int x, ArrayList xs)
{
    for (int i = 0; i < lengthAL(xs); i++)
    {
        if (get(i, xs) == x)
        {
            return true;
        }
    }
    return false;
}

// Indica la cantidad de elementos iguales a x
int apariciones(int x, ArrayList xs)
{
    int apariciones = 0;
    for (int i = 0; i < lengthAL(xs); i++)
    {
        if (get(i, xs) == x)
        {
            apariciones += 1;
        }
    }
    return apariciones;
}

// Crea una nueva lista a partir de la primera y la segunda (en ese orden).
ArrayList append(ArrayList xs, ArrayList ys)
{
    for (int i = 0; i < lengthAL(ys); i++)
    {
        add(get(i, ys), xs);
    }
    return xs;
}

// Devuelve el elemento más chico de la lista
int minimo(ArrayList xs)
{
    int min = get(0, xs);
    for (int i = 1; i < lengthAL(xs); i++)
    {
        int curElem = get(i, xs);
        if (curElem < min)
        {
            min = curElem;
        }
    }

    return min;
}

int main()
{
    ArrayList a = newArrayList();
    ArrayList b = newArrayList();

    add(5, a);
    add(10, a);
    add(99, a);
    add(70, a);

    cout << "min: " << minimo(a) << endl;
    cout << "sumatoria: " << sumatoria(a) << endl;
    sucesores(a);
    cout << "sucesores: " << sumatoria(a) << endl;
    cout << "pertenece: " << pertenece(100, a) << endl;
    cout << "apariciones: " << apariciones(5, a) << endl;

    add(6, a);
    add(6, a);

    cout << "apariciones: " << apariciones(6, a) << endl;

    cout << "append: " << lengthAL(append(b, a)) << endl;

    cout << "min: " << minimo(a) << endl;
    return 0;
}