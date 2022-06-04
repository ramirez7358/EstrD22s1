#include <iostream>
using namespace std;

struct Fraccion
{
    int numerador;
    int denominador;
};

// Propósito: construye una fraccion
// Precondición: el denominador no es cero
Fraccion consFraccion(int numerador, int denominador)
{
    Fraccion f = {numerador, denominador};
    return f;
}

// Propósito: devuelve el numerador
int numerador(Fraccion f)
{
    return f.numerador;
}

// Propósito: devuelve el denominador
int denominador(Fraccion f)
{
    return f.denominador;
}

// Propósito: devuelve el resultado de hacer la división
float division(Fraccion f)
{
    return f.numerador / f.denominador;
}

// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// (sin simplificar)
Fraccion multF(Fraccion f1, Fraccion f2)
{
    Fraccion f = {f1.numerador * f2.numerador, f2.denominador * f2.denominador};
    return f;
}

int getMCD(int a, int b)
{
    while (a != b)
    {
        if (a > b)
        {
            return getMCD(a - b, b);
        }
        else
        {
            return getMCD(a, b - a);
        }
    }
    return a;
}

// Propósito: devuelve una fracción que resulta
// de simplificar la dada por parámetro
Fraccion simplificada(Fraccion p)
{
    int mcd = getMCD(p.numerador, p.denominador);
    Fraccion f = {p.numerador / mcd, p.denominador / mcd};
    return f;
}

int main()
{
    Fraccion f0 = consFraccion(8, 36);
    Fraccion f = simplificada(f0);
    cout << f.numerador << endl;
    cout << f.denominador << endl;
    return 0;
}