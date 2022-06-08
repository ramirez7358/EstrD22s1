#include <iostream>
#include "PersonaBasica.h"
using namespace std;

struct PersonaSt {
  string nombre;
  int    edad;
};

Persona nacer(string n) {
  PersonaSt* p = new PersonaSt;
  p->nombre = n; p->edad = 0;
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

void CumplirAnios(Persona p) {
  p->edad++;
}

void ShowPersona(Persona p) {
  cout << "Persona[" << p << "]("; 
  cout << "nombre <- \"" << p->nombre << "\", ";
  cout << "edad <- " << p->edad;
  cout << ")";
}
