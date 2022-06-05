#include <iostream>
#include "Pokemon.h"
using namespace std;

Pokemon consPokemon(TipoDePokemon tipo)
{
    PokeSt *p = new PokeSt;
    p->tipo = tipo;
    p->vida = 100;
    return p;
}

TipoDePokemon tipoDePokemon(Pokemon p)
{
    return p->tipo;
}

int energia(Pokemon p)
{
    return p->vida;
}

void perderEnergia(int energia, Pokemon p)
{
    p->vida -= energia;
}

bool superaAAux(TipoDePokemon tp1, TipoDePokemon tp2)
{
    return true;
}

bool superaA(Pokemon p1, Pokemon p2)
{
    return superaAAux(p1->tipo, p2->tipo) ? p1 : p2;
}