#include <iostream>
using namespace std;

// Precondición: n <= m
// Devuelve la sumatoria de los numeros entre
// n y m.
int ft(int n, int m)
{
    if (n == m)
    {
        return n;
    }
    return n + ft(n + 1, m);
}

// Ejercicio 2-2
// Precondición: n >= 0
// Devuelve el factorial del numero en n
int fc(int n)
{
    int x = 1;
    while (n > 0)
    {
        x = x * n;
        n--;
    }
    return x;
}

// Ejercicio 2 - 1
// Precondición: c1 < c2
// Imprime todos los valores en ascii de los chars entre c1(inclusive) y c2(inclusive)
void printFromTo(char c1, char c2)
{
    for (int i = 0; c1 + i <= c2; i++)
    {
        cout << c1 + i << ", ";
    }
    cout << endl;
}

// x = 2
// y = 2
void ejercicio1_1()
{
    int x = 0;
    int y = 2;
    x = x + y;

    cout << x << endl;
}

// x = 10
// y = 5
void ejercicio1_2()
{
    int x = 0;
    int y = 0;
    while (y < 5)
    {
        x += y;
        y++;
    }
}

// y = 1
// b = false
void ejercicio1_3()
{
    int y = 0;
    bool b = true;
    while (b)
    {
        y++;
        b = !b;
    }
}

int main()
{
    cout << ft(1, 4) << endl;
    return 0;
}