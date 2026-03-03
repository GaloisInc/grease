CC = ./musl-gcc-docker.sh

.PHONY: all clean

ELFS = $(patsubst %c,%elf,$(wildcard *.c))

all: $(ELFS)

$(ELFS): %.elf: %.c
	$(CC) $(shell ./extract-cflags.py $<) -o $@ $<

clean:
	rm -f $(ELFS)
