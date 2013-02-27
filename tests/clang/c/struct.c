
struct a_struct {
  int field1;
  int field2;
  struct anon {
    int subfield1;
    int subfield2;
  } field3;
};

struct use_a_struct {
  struct a_struct x;
};

void use_field() {
  struct a_struct x;
  x.field1++;
  x.field3.subfield1++;
}
