
struct a_struct {
  int field1;
  int field2;
  int dead_field;

  struct anon {
    int subfield1;
    int subfield2;
    int dead_subfield;
  } field3;
};

struct use_a_struct {
  struct a_struct x;
};

typedef struct a_struct aliast;

void use_field() {
  struct a_struct x;
  x.field1++;
  x.field3.subfield1++;
}

void use_field_via_typedef() {
  //clang provides 2 type versions in the AST, the original one and 
  // one where typedefs have been expanded
  aliast x;
  x.field2++;
}

void use_field_via_typedef2() {
  // unfortunately clang does not expand typedefs here, it works only
  // when the typedef is direct (e.g. not a pointer to a typedef)
  aliast *x;
  // but we record the dependency now also by better managing typedefs ourselves
  x->field2++;
}
