<?php

class :A {
}

class :x:super {
  attribute
    string superstr;
}

class :x:misc {
  attribute
    :x:super,
    string xstr,
    string x-str="1";
}

class :x:require-field {
  attribute
  int req_int @required,
  string req_str @required;
}