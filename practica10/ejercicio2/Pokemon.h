#include <iostream>
using namespace std;

typedef string TipoDePokemon;

struct PokeSt
{
    TipoDePokemon tipo;
    int vida;
};

typedef PokeSt *Pokemon;

struct EntrenadorSt
{
    string nombre;
    Pokemon *pokemon;
    int cantPokemon;
};

typedef EntrenadorSt *Entrenador;

Pokemon consPokemon(TipoDePokemon tipo);
TipoDePokemon tipoDePokemon(Pokemon p);
int energia(Pokemon p);
void perderEnergia(int energia, Pokemon p);
bool superaA(Pokemon p1, Pokemon p2);