CC=gcc
CFLAGS=-Wall -pedantic -ggdb -g3
DEPS=cel.h
OBJ=cel.o 

all: life 

%.o: %.c $(DEPS)
	$(CC) $(CFLAGS) -c -o $@ $<

life: $(OBJ) life.c
	$(CC) $(CFLAGS) $^ -o $@

random: $(OBJ) random.c
	$(CC) $(CFLAGS) $^ -o $@

.PHONY: clean

clean:
	rm -Rf *.o life random
