#include <iostream>
using namespace std;
#include "Mascota.h"

struct PersonaSt;

typedef PersonaSt* Persona;

Persona nacer(string n);
void    Morir(Persona p);
string  nombre(Persona p);
int     edad(Persona p);
Mascota mascotaNro(Persona p, int i);
void    CumplirAnios(Persona p);
void    AdoptarNuevaMascota(Persona p, Mascota m);
void    LiberarMascota(Persona p, string nombre);
void    ShowPersona(Persona p);
