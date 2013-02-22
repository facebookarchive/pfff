
struct a_struct {
  int field1;
  int field2;
  struct anon {
    int subfield1;
    int subfield2;
  };
};

struct use_a_struct {
  struct a_struct x;
};


