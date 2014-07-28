
CC=gcc -Wmain-return-type

all: fields.exe constants.exe function_pointer.exe

%.exe: %.c
	$(CC) $^ -o $@

clean:
	rm -f *.exe