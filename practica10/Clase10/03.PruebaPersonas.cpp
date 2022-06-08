#include <iostream>
using namespace std;
#include "PersonaBasica.h"

int main() {
    Persona carlos = nacer("Carlitos");
    CumplirAnios(carlos);
    ShowPersona(carlos);  cout << endl;
    Persona juan = nacer("Juancito");
    ShowPersona(juan);
}