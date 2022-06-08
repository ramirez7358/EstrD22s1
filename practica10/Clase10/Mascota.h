#include <iostream>
using namespace std;

struct MascotaSt;

typedef MascotaSt* Mascota;

Mascota mascotaNula();
Mascota nacerMascota(string n, string h);
void    MorirMascota(Mascota m);
string  nombreMascota(Mascota m);
string  habilidad(Mascota m);
void    ShowMascota(Mascota m);
