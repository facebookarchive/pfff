include ../../Makefile.config


all:
	cat packed.piqi | sed -e 's/\.protobuf-packed//' > unpacked.piqi
	cat test-all.piq | sed -e 's/:packed/:unpacked/' > test-all-unpacked.piq
	./test


clean:
	rm -f *.pb *.proto* *.pib unpacked.piqi test-all-unpacked.piq

