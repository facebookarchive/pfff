SRC=Clone.java TestException.java \
    NestedClass.java GenericClass.java AnonClass.java \
    AnEnum.java AnAnnotation.java

OBJS=$(SRC:.java=.class)

all: graph_code.marshall

.PHONY: graph_code.marshall

graph_code.marshall: $(OBJS)
	~/pfff/codegraph -symlinks -lang bytecode -build .

prolog:
	~/pfff/codequery -lang bytecode -build .

.SUFFIXES: .java .class

.java.class:
	javac $<

clean:
	rm -f *.class graph_code.*