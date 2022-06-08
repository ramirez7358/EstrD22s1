#include <iostream>
#include "PersonaConUnaMascota.h"
#include "Mascota.h"
using namespace std;

struct PersonaSt {
  string  nombre;
  int     edad;
  Mascota mascota;
};

Persona nacer(string n) {
  PersonaSt* p = new PersonaSt;
  p->nombre = n; p->edad = 0;
  p->mascota = mascotaNula();
  return p;
}

void Morir(Persona p) {
  delete p;
}

string nombre(Persona p) {
  return p->nombre;
}

int edad(Persona p) {
  return p->edad;
}

Mascota mascota(Persona p) {
  return p->mascota;
}

void CumplirAnios(Persona p) {
  p->edad++;
}

void AdoptarMascota(Persona p, Mascota m) {
  p->mascota = m;
}

void LiberarMascota(Persona p) {
  p->mascota = mascotaNula();
}

void ShowPersona(Persona p) {
  cout << "Persona[" << p << "]("; 
  cout << "nombre <- \"" << p->nombre << "\", ";
  cout << "edad <- " << p->edad << endl;
  cout << "                 " << ",";
  cout << "mascota <- ";  
  ShowMascota(p->mascota);
  cout << ")";
}
