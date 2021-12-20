SRC = tests/dummy.cpp \
	tests/regex_tests.cpp

test: $(SRC)
	g++ -std=c++17 -o ./bin/testbin $(SRC) && ./bin/testbin


clean:
	rm ./bin/testbin
