#include <iostream>
using namespace std;
#include "PersonaBasica.h"

int main() {
    Persona carlos = nacer("Carlitos");
    CumplirAnios(carlos);
    ShowPersona(carlos);   cout << endl;
    Morir(carlos);
    ShowPersona(carlos);  // Acceso ilegal a un espacio no reservado...
}