#include <iostream>
using namespace std;
#include "PersonaBasica.h"

int main() {
    Persona carlos = nacer("Carlitos");
    CumplirAnios(carlos);
    ShowPersona(carlos);  cout << endl;
    carlos = nacer("Juancito"); // Carlitos es inaccesible desde este punto (memory leak!)
    ShowPersona(carlos);

}