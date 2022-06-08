#include <iostream>
using namespace std;
#include "Celda.h"
#include "Tablero.h"

int main() {
  Tablero t = tableroVacio(3,4);
  Poner(t, Rojo);
  //ShowTablero(t); cout << endl;
  ShowTableroPretty(t);
}
