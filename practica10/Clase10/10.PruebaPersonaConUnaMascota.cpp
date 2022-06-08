#include <iostream>
using namespace std;
#include "PersonaConUnaMascota.h"
#include "Mascota.h"

int main() {
  Persona constanza = nacer("Constanza");
  Persona fabiana = nacer("Fabiana");
  Mascota gatucha = nacerMascota("Gatucha", "ronronear");
  AdoptarMascota(constanza, gatucha);
  AdoptarMascota(fabiana, gatucha);
  MorirMascota(mascota(constanza));  
  ShowPersona(fabiana);  cout << endl;
  ShowPersona(constanza);
}
