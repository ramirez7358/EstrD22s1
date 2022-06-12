#include <iostream>
using namespace std;

struct PokeSt;

typedef string TipoDePokemon;
typedef PokeSt *Pokemon;

Pokemon consPokemon(TipoDePokemon tipo);
TipoDePokemon tipoDePokemon(Pokemon p);
int energia(Pokemon p);
void perderEnergia(int energia, Pokemon p);
bool superaA(Pokemon p1, Pokemon p2);