#include <iostream>
using namespace std;
#include "Mascota.h"

struct PersonaSt;

typedef PersonaSt *Persona;

Persona nacer(string n);
void Morir(Persona p);
string nombre(Persona p);
int edad(Persona p);
Mascota mascota(Persona p);
void CumplirAnios(Persona p);
void AdoptarMascota(Persona p, Mascota m);
void LiberarMascota(Persona p);
void ShowPersona(Persona p);
