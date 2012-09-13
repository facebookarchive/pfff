<?php

// people should not abuse the case insensivity of PHP

// used in phabricator

const True = 1;
const False = 0;

class STDClass { }
class StdClass { }

function pathInfo($path, $opt = 15) { }
