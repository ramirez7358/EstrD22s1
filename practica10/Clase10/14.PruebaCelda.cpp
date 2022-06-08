#include <iostream>
using namespace std;
#include "Celda.h"

int main() {
  Celda c = celdaVacia();
  PonerEnCelda(c, Rojo);  PonerEnCelda(c, Rojo);  PonerEnCelda(c, Rojo);
  PonerEnCelda(c, Verde); PonerEnCelda(c, Verde); PonerEnCelda(c, Verde);
  PonerEnCelda(c, Negro);
  SacarEnCelda(c, Rojo);
  // SacarEnCelda(c, Azul);
  cout << "nroBolitas(Azul)  = " << nroBolitasEnCelda(c, Azul)  << endl;
  cout << "nroBolitas(Negro) = " << nroBolitasEnCelda(c, Negro) << endl;
  cout << "nroBolitas(Rojo)  = " << nroBolitasEnCelda(c, Rojo)  << endl;
  cout << "nroBolitas(Verde) = " << nroBolitasEnCelda(c, Verde) << endl;
  ShowCelda(c); cout << endl;
  ShowCeldaPretty(c);
}
