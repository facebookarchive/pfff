CLANG=/home/pad/local/clang_ast/clang-llvm/llvm/Debug+Asserts/bin/clang-check
CG=../../../codegraph.opt

# have also to update compile_commands.json
SRC=foo.c bar.c typedef.c struct.c enum.c define.c global.c add.c

OBJS=$(SRC:.c=.clang)

all: $(OBJS)
	$(CG) -lang clang -build .



.SUFFIXES: .c .clang

.c.clang:
	$(CLANG) --ast-dump $^ > $@

clean:
	rm -f *.clang
