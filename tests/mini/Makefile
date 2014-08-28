
CC=gcc -Wmain-return-type

SRC= \
  pointer.c \
  array.c \
  constants.c \
  deref.c \
  fields.c \
  function.c \
  function_pointer.c \
  on_the_fly.c \
  combination.c \

#  grammar.c \

EXE=

all: $(SRC:.c=.exe)

%.exe: %.c
	$(CC) $^ -o $@

clean::
	rm -f *.exe


.PHONY: datalog

datalog:
	../../codequery -lang c -datalog -build .

clean::
	rm -f facts.*
	rm prolog_compiled_db pfff.log

