CLANG=/home/pad/local/clang_ast/clang-llvm/llvm/Debug+Asserts/bin/clang-check
CG=../../../codegraph.opt

SRC=foo.c bar.c

OBJS=$(SRC:.c=.clang)

all: $(OBJS)
	$(CG) -lang clang -build .



.SUFFIXES: .c .clang

.c.clang:
	$(CLANG) --ast-dump $^ > $@

clean:
	rm -f *.clang
