GHC = ghc
CXX = clang++

CXXFLAGS = -std=c++11 -O3
INCLUDEDIRS = pcg-cpp/include

GHCINCLUDES ?= ../src

run: RefTest
	./RefTest

RefTest: RefTest.hs wrapper.o
	$(GHC) -main-is RefTest -i$(patsubst %,:%, $(GHCINCLUDES)) $^ -o $@ \
		-optl-lstdc++  # link cxx runtime

%.o: %.cpp
	$(CXX) $(CXXFLAGS) $(patsubst %,-I%, $(INCLUDEDIRS)) -c $^ -o $@

clean:
	rm -f wrapper.o RefTest.o RefTest.hi RefTest
