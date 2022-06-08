#include <iostream>
#include "Contador.h"
using namespace std;

Contador crearContador() {
  int* c = new int;
  *c = 0;
  return c;
}

void BorrarContador(Contador c) {
  delete c;
}

int leerContador(Contador c) {
  return *c;
}

void Incrementar(Contador c) {
  (*c)++;
}

