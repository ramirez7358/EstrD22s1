#include <iostream>
using namespace std;
#include "Contador.h"

int main() {
    Contador c = crearContador();
    cout << "El valor del contador c en " << c << " es " << leerContador(c) << endl;
    Incrementar(c);
    cout << "El valor del contador c en " << c << " es " << leerContador(c) << endl;
    Incrementar(c);
    cout << "El valor del contador c en " << c << " es " << leerContador(c) << endl;
    BorrarContador(c);
    cout << "Murio el contador..." << endl;
    Contador d = crearContador();
    c = crearContador();
    Incrementar(c);
    cout << "El valor del contador d en " << d << " es " << leerContador(d) << endl;
    cout << "El valor del contador c en " << c << " es " << leerContador(c) << endl;
}