CLANG=/home/pad/local/clang_ast/clang-llvm/llvm/Debug+Asserts/bin/clang-check


SRC=foo.c

OBJS=$(SRC:.c=.clang)

all: $(OBJS)


.SUFFIXES: .c .clang

.c.clang:
	$(CLANG) --ast-dump $^ > $@



