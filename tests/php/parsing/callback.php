<?php

foo('callback1', 1);
foo('A::callback2', 1);
foo(array('A', 'callback3'), 1);
foo(array($this, 'callback4'), 1);
foo(array($object, 'callback5'), 1);
