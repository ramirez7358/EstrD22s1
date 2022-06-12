#include <iostream>
#include "Entrenador.h"
#include "Pokemon.h"
using namespace std;

int main()
{
    Pokemon p = consPokemon("fuego");
    Pokemon p2 = consPokemon("planta");
    Pokemon p3 = consPokemon("agua");

    Pokemon list[3];
    list[0] = p;
    list[1] = p2;
    list[2] = p3;

    Pokemon list2[1];
    list2[0] = p;

    Entrenador e = consEntrenador("Leonel", 1, list);
    Entrenador e2 = consEntrenador("Brian", 3, list2);

    cout << leGanaATodos(e, e2) << endl;

    return 0;
}