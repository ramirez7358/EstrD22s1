#include <iostream>
using namespace std;

struct PersonaSt;

typedef PersonaSt* Persona;

Persona nacer(string n);
void    Morir(Persona p);
string  nombre(Persona p);
int     edad(Persona p);
void    CumplirAnios(Persona p);
void    ShowPersona(Persona p);
