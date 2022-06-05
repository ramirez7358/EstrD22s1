#include <iostream>
#include "Pokemon.cpp"
using namespace std;

int main()
{
    Pokemon p = consPokemon("fuego");

    cout << p->tipo << endl;

    return 0;
}