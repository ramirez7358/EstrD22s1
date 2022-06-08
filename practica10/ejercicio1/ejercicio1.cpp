#include <iostream>
#include "Persona.cpp"
using namespace std;

int main()
{
    Persona p = consPersona("Brian", 26);

    cout << nombre(p) << endl;
    cout << edad(p) << endl;
    crecer(p);
    cambioDeNombre("Leonel", p);
    cout << edad(p) << endl;
    cout << nombre(p) << endl;
}