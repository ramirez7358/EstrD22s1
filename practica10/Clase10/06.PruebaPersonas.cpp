#include <iostream>
using namespace std;
#include "PersonaBasica.h"

int main() {
    Persona carlos = nacer("Carlitos");
    CumplirAnios(carlos);
    ShowPersona(carlos);  cout << endl;
    Morir(carlos);
    carlos = nacer("Juancito");
    ShowPersona(carlos);
}