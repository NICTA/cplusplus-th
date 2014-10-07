#include <stdlib.h>
#include <string>
#include <iostream>
#include <fstream>

using namespace std;

namespace haskell {

string* fromCString(char const* x, int length) {
  return new string(x, length);
}

char const* toCString(string const &x) {
  return x.c_str();
}

int cstringLen(string const &x) {
  return x.length();
}

// TODO: replace with an FFI call to libc++
void deleteString(string const *x) {
  delete x;
}

}
