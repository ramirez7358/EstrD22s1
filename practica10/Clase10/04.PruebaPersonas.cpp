#include <iostream>
using namespace std;
#include "PersonaBasica.h"

int main() {
    Persona carlos = nacer("Carlitos");
    CumplirAnios(carlos);
    ShowPersona(carlos);    cout << endl;
    Persona carlosJr = nacer("Carlitos");
    CumplirAnios(carlosJr);
    ShowPersona(carlosJr);  cout << endl;
    cout << endl;
    ShowPersona(carlos);    cout << endl;
    CumplirAnios(carlosJr);
    ShowPersona(carlosJr);
}