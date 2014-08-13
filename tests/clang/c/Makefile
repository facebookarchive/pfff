TOP=../../..
CLANG=clang-check
#CLANG=/home/pad/local/clang_ast/clang-llvm/llvm/Debug+Asserts/bin/clang-check
CG=$(TOP)/codegraph
CQ=$(TOP)/codequery
PFFF=$(TOP)/pfff

#coupling: have also to update compile_commands.json!!
SRC=foo.c bar.c typedef.c struct.c enum.c define.c global.c add.c \
 static1.c static2.c function_pointer.c struct_functions.c \
 bool.c sizeof.c extern.c initializer.c assign.c

OBJS=$(SRC:.c=.clang)

all: $(OBJS)
	$(PFFF)_test -uninclude_clang .
	$(CG) -lang clang2 -build .
	cat pfff.log

all2: $(SRC)
	$(CG) -lang c -build .
	cat pfff.log

prolog: $(OBJS)
	$(CQ) -lang clang2 -build .

.SUFFIXES: .c .clang

.c.clang:
	$(CLANG) --ast-dump $^ > $@

clean:
	rm -f *.clang *.clang2
