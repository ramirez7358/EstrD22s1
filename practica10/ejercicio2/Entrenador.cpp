#include <iostream>
#include "Entrenador.h"
using namespace std;

struct EntrenadorSt
{
    string nombre;
    Pokemon *pokemon;
    int cantPokemon;
};

Entrenador consEntrenador(string nombre, int cantidad, Pokemon *pokemon)
{
    EntrenadorSt *e = new EntrenadorSt;
    e->nombre = nombre;
    e->cantPokemon = cantidad;
    e->pokemon = pokemon;
    return e;
}

string nombreDeEntrenador(Entrenador e)
{
    return e->nombre;
}

int cantidadDePokemon(Entrenador e)
{
    return e->cantPokemon;
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e)
{
    int counter;

    for (int i = 0; i < e->cantPokemon; i++)
    {
        if (tipoDePokemon(e->pokemon[i]) == tipo)
        {
            counter++;
        }
    }

    return counter;
}

Pokemon pokemonNro(int i, Entrenador e)
{
    return e->pokemon[i];
}

bool hayUnoQueLeGana(Pokemon pk, Pokemon *pks)
{
    for (int i = 0; i < sizeof(pks); i++)
    {
        if (superaA(pks[i], pk))
        {
            return true;
        }
    }
    return false;
}

bool leGanaATodos(Entrenador e1, Entrenador e2)
{
    for (int i = 0; i < sizeof(e2->pokemon); i++)
    {
        if (hayUnoQueLeGana(e2->pokemon[i], e1->pokemon))
        {
            return true;
        }
    }
    return false;
}