ctest: piqi.piqi.pb.o test.cpp
	$(CXX) $(CXXFLAGS) -o $@ $^ -lprotobuf -lpthread

clean:
	rm -f *.o ctest
