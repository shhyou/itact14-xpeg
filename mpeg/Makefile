-include Makefile.common

CXXFLAGS = -std=c++11 -MMD -g -DDEBUG_LEVEL=5 -Wall -Wshadow -Wextra -Wconversion -pedantic -Iinclude
#CXXFLAGS = -std=c++11 -MMD -O2 -DDEBUG_LEVEL=0 -Wall -Wshadow -Wextra -Wconversion -pedantic -Iinclude
CXX = g++

OBJS = \
	bin/bmp \
	bin/input_stream \
	bin/mpeg

TARGET = mpeg$(exe)

.PHONY: all
all: $(TARGET)

.PHONY: clean
clean:
	-$(RM) $(TARGET) $(call fixpath,bin/*)

# Compile the target file (e.g. hw3_1.exe)
$(TARGET): %$(exe): bin/%.o $(OBJS:=.o)
	$(CXX) $(CXXFLAGS) -o $@ $^

# Compile library files, e.g. `instruction`, `tokenizer`
bin/%.o: src/%.cpp
	$(call if_not_exist,bin,mkdir bin)
	$(CXX) $(CXXFLAGS) -c -o $@ $<

# Include compiler-generated dependency files
-include $(OBJS:=.d)
