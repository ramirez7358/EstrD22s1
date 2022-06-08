#include <iostream>
#include <iomanip>
#include "Celda.h"
using namespace std;

struct CeldaSt {
  int azul;
  int negro;
  int rojo;
  int verde;
};

Celda celdaVacia() {
  CeldaSt* c = new CeldaSt;
  c->azul = 0; c->negro = 0;
  c->rojo = 0; c->verde = 0;
  return c;
}

void  BorrarCelda(Celda c) {
  delete c;
}

void  PonerEnCelda(Celda c, Color color) {
  switch (color) {
    case Azul : { c->azul++;  break; }
    case Negro: { c->negro++; break; }
    case Rojo : { c->rojo++;  break; }
    case Verde: { c->verde++; break; }
    default   : perror("Poner debe recibir un color válido");
  }
}

void  SacarEnCelda(Celda c, Color color) {
  switch (color) {
    case Azul : { if (c->azul  > 0) { c->azul--;  } else { perror("No hay bolitas azules"); }
                  break; }
    case Negro: { if (c->negro > 0) { c->negro--; } else { perror("No hay bolitas negras"); } 
                  break; }
    case Rojo : { if (c->rojo  > 0) { c->rojo--;  } else { perror("No hay bolitas rojas"); } 
                  break; }
    case Verde: { if (c->verde > 0) { c->verde--; } else { perror("No hay bolitas verdes"); } 
                  break; }
    default   : perror("Sacar debe recibir un color válido");
  }
}

int   nroBolitasEnCelda(Celda c, Color color) {
  switch (color) {
    case Azul : return(c->azul);
    case Negro: return(c->negro);
    case Rojo : return(c->rojo);
    case Verde: return(c->verde);
    default   : perror("nroBolitas debe recibir un color válido");
  }
}

bool  hayBolitasEnCelda(Celda c, Color color) {
  switch (color) {
    case Azul : return(c->azul  == 0);
    case Negro: return(c->negro == 0);
    case Rojo : return(c->rojo  == 0);
    case Verde: return(c->verde == 0);
    default   : perror("hayBolitas debe recibir un color válido");
  }
}

void ShowCeldaPretty(Celda c) {
  cout << "|--------------|" << endl;
  cout << "| A: " << setw(2) << c->azul << "  N: " << setw(2) << c->negro << " |" << endl;
  cout << "| R: " << setw(2) << c->rojo << "  V: " << setw(2) << c->verde << " |" << endl;
  cout << "|--------------|" << endl;
}

void ShowCelda(Celda c) {
  cout << "Celda[" << c << "](";
  cout << "a<-" << setw(1) << c->azul  << ",";
  cout << "n<-" << setw(1) << c->negro << ",";
  cout << "r<-" << setw(1) << c->rojo  << ",";
  cout << "v<-" << setw(1) << c->verde;
  cout << ")";
}