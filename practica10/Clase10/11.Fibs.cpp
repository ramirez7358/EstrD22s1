#include <iostream>
using namespace std;

int fib(int n) { // PRECOND: n>0
  int fibn[n];
  fibn[0] = 1;  fibn[1] = 1;
  for (int i=2; i<n; i++) {
    fibn[i] = fibn[i-2] + fibn[i-1];
  }
  return(fibn[n-1]);
}

int main() {
  cout << "fib(4) = " << fib(4) << endl;
  cout << "fib(5) = " << fib(5) << endl;
  cout << "fib(6) = " << fib(6) << endl;
}


