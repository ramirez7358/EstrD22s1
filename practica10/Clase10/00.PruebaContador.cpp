#include <iostream>
using namespace std;

typedef int* Contador;

Contador crearContador() {
  int* punteroANum = new int;
  *punteroANum = 0;
  return punteroANum;
}

int main() {
    Contador m = crearContador();
    Contador n = crearContador();
    (*m)++;
    cout << "Lo que esta en la posicion " << m << " es " << *m << endl;
    cout << "Lo que esta en la posicion " << n << " es " << *n << endl;
}