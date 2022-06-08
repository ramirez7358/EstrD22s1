#include <iostream>
#include "PersonaConMascotas.h"
#include "Mascota.h"
using namespace std;

struct PersonaSt {
  string   nombre;
  int      edad;
  int      maxMascotas;
  int      cantMascotas;
  Mascota* mascotas;
};

Persona nacer(string n) {
  PersonaSt* p = new PersonaSt;
  p->nombre = n; p->edad = 0;
  p->maxMascotas = 2; p->cantMascotas = 0;
  p->mascotas = new Mascota[p->maxMascotas];
  return p;
}

void Morir(Persona p) {
  delete p->mascotas;
  delete p;
}

string nombre(Persona p) {
  return p->nombre;
}

int edad(Persona p) {
  return p->edad;
}

Mascota mascotaNro(Persona p, int i) {
  // PRECOND: 0 < i <= p->cantMascotas
  if (i > 0 && i <= p->cantMascotas) {
    return p->mascotas[i-1];
  }
  return mascotaNula();
}

void CumplirAnios(Persona p) {
  p->edad++;
}

void DuplicarEspacioDeMascotas(Persona p) {
  Mascota* temp = new Mascota[p->maxMascotas*2];
  for(int i=0; i<p->maxMascotas; i++)
    { temp[i] = p->mascotas[i]; }
  delete p->mascotas;
  p->maxMascotas = p->maxMascotas*2;
  p->mascotas = temp;
}

void AdoptarNuevaMascota(Persona p, Mascota m) {
  if (p->cantMascotas == p->maxMascotas)
    { DuplicarEspacioDeMascotas(p); }
  p->mascotas[p->cantMascotas++] = m;
}

void LiberarMascota(Persona p, string nombre) {
  int i = 0;
  while (i < p->cantMascotas 
      && nombreMascota(p->mascotas[i]) != nombre) {
    i++;
  }  
  if (i < p->cantMascotas) {
    p->cantMascotas--; 
  }
  while (i < p->cantMascotas-1) {
    p->mascotas[i] = p->mascotas[i+1];
  }
}

void ShowPersona(Persona p) {
  cout << "Persona[" << p << "," << p->cantMascotas << "," << p->maxMascotas << "]("; 
  cout << "nombre <- \"" << p->nombre << "\", ";
  cout << "edad <- " << p->edad << endl;
  cout << "                     " << ",";
  cout << "mascotas[" << p->mascotas << "] <- [ ";
  for (int i=0; i < p->cantMascotas; i++) {  
    cout << endl << "                         ";
    if (i>0) { cout << ", "; } else { cout << "  "; }
    ShowMascota(p->mascotas[i]);
  }
  cout << endl << "                     ]";
  cout << ")";
}

