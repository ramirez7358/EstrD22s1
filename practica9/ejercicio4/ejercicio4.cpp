#include <iostream>
using namespace std;

// Propósito: imprime n veces un string s.
void printNIterative(int n, string s)
{
    while (n > 0)
    {
        cout << s << endl;
        n--;
    }
}

void printNRecursive(int n, string s)
{
    if (n != 0)
    {
        cout << s << endl;
        printNRecursive(n - 1, s);
    }
}

// Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
void cuentaRegresivaIterative(int n)
{
    while (n >= 0)
    {
        cout << n << endl;
        n--;
    }
}

void cuentaRegresivaRecursive(int n)
{
    if (n >= 0)
    {
        cout << n << endl;
        cuentaRegresivaRecursive(n - 1);
    }
}

// Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
void desdeCeroHastaNIterative(int n)
{
    int counter = 0;
    while (counter <= n)
    {
        cout << counter << endl;
        counter++;
    }
}

void desdeCeroHastaNRecursive2(int from, int to)
{
    if (from <= to)
    {
        cout << from << endl;
        desdeCeroHastaNRecursive2(++from, to);
    }
}

void desdeCeroHastaNRecursive(int n)
{
    desdeCeroHastaNRecursive2(0, n);
}

// Propósito: realiza la multiplicación entre
// dos números (sin utilizar la operación * de C++).
int multIterative(int n, int m)
{
    int result = 0;
    while (m != 0)
    {
        result += n;
        m--;
    }

    return result;
}

int multRecursive(int n, int m)
{
    if (m > 0)
    {
        return n + multRecursive(n, m - 1);
    }
    else
    {
        return 0;
    }
}

// Propósito: imprime los primeros n char del string s, separados por un salto de línea.
// Precondición: el string tiene al menos n char.
void primerosNIterative(int n, string s)
{
    int i = 0;
    while (n > 0)
    {
        cout << s[i] << endl;
        n--;
        i++;
    }
}

void primerosNResursive2(int from, int to, string s)
{
    if (from <= to)
    {
        cout << s[from] << endl;
        primerosNResursive2(++from, to, s);
    }
}

void primerosNRecursive(int n, string s)
{
    primerosNResursive2(0, n, s);
}

// Propósito: indica si un char c aparece en el string s.
bool perteneceIterative(char c, string s)
{
    bool ret = false;
    for (int i = 0; i < s.length(); i++)
    {
        if (c == s[i])
        {
            ret = true;
            break;
        }
    }
    return ret;
}

bool perteneceRecursive2(int from, int to, char c, string s)
{
    if (from <= to)
    {
        return (s[from] == c) || perteneceRecursive2(++from, to, c, s);
    }
    else
    {
        return false;
    }
}

bool perteneceRecursive(char c, string s)
{
    return perteneceRecursive2(0, s.length(), c, s);
}

int aparicionesIterative(char c, string s)
{
    int apariciones = 0;
    for (int i = 0; i < s.length(); i++)
    {
        if (c == s[i])
        {
            apariciones++;
        }
    }
    return apariciones;
}

int aparicionesRecursive2(int length, char c, string s)
{
    if (length != 0)
    {
        if (c == s[length])
        {
            return 1 + aparicionesRecursive2(--length, c, s);
        }
        else
        {
            return aparicionesRecursive2(--length, c, s);
        }
    }
    else
    {
        return 0;
    }
}

int aparicionesRecursive(char c, string s)
{
    return aparicionesRecursive2(s.length(), c, s);
}

int main()
{
    cout << aparicionesRecursive('a', "Brian Ramireza") << endl;
    return 0;
}