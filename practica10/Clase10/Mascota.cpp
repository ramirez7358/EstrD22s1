#include <iostream>
#include "Mascota.h"
using namespace std;

struct MascotaSt {
  string nombre;
  string habilidad;
};

Mascota mascotaNula() {
  return NULL;
}

Mascota nacerMascota(string n, string h) {
  MascotaSt* m = new MascotaSt;
  m->nombre = n; m->habilidad = h;
  return m;
}

void MorirMascota(Mascota m) {
  delete m;
}

string nombreMascota(Mascota m) {
  return m->nombre;
}

string habilidad(Mascota m) {
  return m->habilidad;
}

void ShowMascota(Mascota m) {
  if (m!=NULL) {
    cout << "Mascota[" << m << "]("; 
    cout << "nombre <- \"" << m->nombre << "\", ";
    cout << "habilidad <- \"" << m->habilidad << "\"";
    cout << ")";
  } else { cout << "<>"; }
}
