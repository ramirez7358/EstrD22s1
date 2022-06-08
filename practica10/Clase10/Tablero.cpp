#include <iostream>
#include <iomanip>
using namespace std;
#include "Celda.h"
#include "Tablero.h"

struct TableroSt {
  int maxCol;    int maxRow;
  int curCol;    int curRow;
  Celda* celdas;
};

int index(Tablero t, int col, int row) {
  return(row + col * t->maxRow);
}

Tablero tableroVacio(int columnas, int filas) {
  TableroSt* t = new TableroSt;
  t->maxCol = columnas; t->maxRow = filas;
  t->curCol = 1;        t->curRow = 2;
  t->celdas = new Celda[t->maxCol * t->maxRow];
  for(int c=0; c<t->maxCol; c++) {
    for(int f=0; f<t->maxRow; f++) {
      t->celdas[index(t,c,f)] = celdaVacia();
    }
  }
  return t;
}

void Poner(Tablero t, Color color) {
  PonerEnCelda(t->celdas[index(t, t->curCol, t->curRow)], color);
}


void ShowCeldaSepTop(int c, int r, Tablero t) {
  if (c == t->curCol 
   && r == t->curRow)
       { cout << "==============|";}
  else 
  { cout << "--------------|"; }
}

void ShowCeldaSep(int c, int r, Tablero t) {
  if (c == t->curCol 
   && (r == t->curRow 
    || r == t->curRow + 1))
       { cout << "==============|";}
  else
   { cout << "--------------|"; }
}

void ShowCeldaLine(string letra1, int nro1, string letra2, int nro2) {
  cout << " " << letra1 << ": " << setw(2) << nro1 << "  " << letra2 << ": " << setw(2) << nro2 << " |";
}

void ShowTableroPretty(Tablero t) {
  Celda curCelda;
  cout << "|"; for(int c=0; c<t->maxCol; c++) { ShowCeldaSepTop(c,0,t); }; cout << endl;
  for(int f=t->maxRow-1; f>=0; f--) {
      cout << "|";
      for(int c=0; c<t->maxCol; c++) { 
        curCelda = t->celdas[index(t,c,f)];
        ShowCeldaLine("A", nroBolitasEnCelda(curCelda,Azul)
                     ,"N", nroBolitasEnCelda(curCelda,Negro)); 
      }
      cout << endl;

      cout << "|";
      for(int c=0; c<t->maxCol; c++) { 
        curCelda = t->celdas[index(t,c,f)];
        ShowCeldaLine("R", nroBolitasEnCelda(curCelda,Rojo)
                     ,"V", nroBolitasEnCelda(curCelda,Verde)); 
      }
      cout << endl;
      
      cout << "|"; for(int c=0; c<t->maxCol; c++) { ShowCeldaSep(c,f,t); }; cout << endl;
  }
}

void ShowTablero(Tablero t) {
  cout << "Tablero[" << endl;
  for(int f=t->maxRow-1; f>=0; f--) {
    for(int c=0; c<t->maxCol; c++) {
      if(c!=0) { cout << ", "; }
      ShowCelda(t->celdas[index(t,c,f)]);
    }
    cout << endl;
  }
  cout << "]" << endl;
}
