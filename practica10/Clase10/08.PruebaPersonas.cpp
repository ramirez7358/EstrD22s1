#include <iostream>
using namespace std;
#include "PersonaBasica.h"

int main() {
    Persona carlos = nacer("Carlitos");
    CumplirAnios(carlos);
    ShowPersona(carlos);  cout << endl;
    Morir(carlos);
    Persona juan = nacer("Juancito");
    ShowPersona(carlos);    // Acceso ilegal a una memoria que no es mía...
}