CLANG=/home/pad/local/clang_ast/clang-llvm/llvm/Debug+Asserts/bin/clang-check
CG=../../../codegraph.opt
PFFF=../../../pfff

# have also to update compile_commands.json
SRC=foo.c bar.c typedef.c struct.c enum.c define.c global.c add.c \
 static1.c static2.c

OBJS=$(SRC:.c=.clang)

all: $(OBJS)
	$(PFFF) -uninclude_clang
	$(CG) -lang clang2 -build .



.SUFFIXES: .c .clang

.c.clang:
	$(CLANG) --ast-dump $^ > $@

clean:
	rm -f *.clang
