typedef int a_typedef;

void use_typedef() {
  a_typedef local;
  local++;
  return;
}


typedef int conflict;
// this is actually forbidden by the compiler
//typedef float conflict;


// this leads to AnEnum being typedefed to ... itself :(
typedef enum {
  ANOTHER1
} AnEnum;
