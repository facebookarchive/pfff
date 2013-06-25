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
    int xint=1,
    var xvar,
    enum { "cool", "lame" } xenum;
}
