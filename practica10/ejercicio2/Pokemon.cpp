#include <iostream>
#include "Pokemon.h"
using namespace std;

struct PokeSt
{
    TipoDePokemon tipo;
    int vida;
};

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
    if (tp1 == "agua" && tp2 == "fuego")
    {
        return true;
    }
    else if (tp1 == "fuego" && tp2 == "planta")
    {
        return true;
    }
    else if (tp1 == "planta" && tp2 == "agua")
    {
        return true;
    }
    else
    {
        return false;
    }
}

bool superaA(Pokemon p1, Pokemon p2)
{
    return superaAAux(p1->tipo, p2->tipo);
}