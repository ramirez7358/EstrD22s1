#include <iostream>
using namespace std;

int* countdown(int n, int j) { 
  // PRECOND: j<n
  int* ns = new int[n-j+1];
  for (int i=0; i<=n-j; i++) {
    ns[i] = n-i;
  }
  return ns;
}

int main() {
  int* ns = countdown(10,6);
  for (int t=0; t<5; t++) {
    cout << ns[t] << " ";
  }
  cout << endl;
}


