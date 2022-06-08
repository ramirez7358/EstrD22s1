#include <iostream>
using namespace std;
#include "PersonaConMascotas.h"
#include "Mascota.h"

int main() {
  Persona constanza = nacer("Constanza");
  AdoptarNuevaMascota(constanza, nacerMascota("Gatucha", "ronronear"));
  AdoptarNuevaMascota(constanza, nacerMascota("Dinamita", "recuperar palitos"));
  AdoptarNuevaMascota(constanza, nacerMascota("Nemo", "aleta feliz"));
  ShowPersona(constanza);
}
