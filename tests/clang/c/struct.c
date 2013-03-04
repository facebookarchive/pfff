
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
  aliast x;
  x.field2++;
}
