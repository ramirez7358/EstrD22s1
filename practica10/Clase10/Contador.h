#include <iostream>
using namespace std;

typedef int* Contador;

Contador crearContador();
void     BorrarContador(Contador c);
int      leerContador(Contador c);
void     Incrementar(Contador c);
