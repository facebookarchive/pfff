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
