#include <iostream>
using namespace std;
#include "Contador.h"

int main() {
    Contador c = crearContador();
    cout << "El valor del contador en " << c << " es " << leerContador(c) << endl;
    Incrementar(c);
    cout << "El valor del contador en " << c << " es " << leerContador(c) << endl;
    Incrementar(c);
    cout << "El valor del contador en " << c << " es " << leerContador(c) << endl;
}