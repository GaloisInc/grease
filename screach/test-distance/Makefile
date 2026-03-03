CC=x86_64-linux-gnu-gcc
CFLAGS=-g -O0
CLANG=clang


dist-test.exe: dist-test.c
	$(CC) $(CFLAGS) -shared -o $@  $^


# the gcc i have doesnt do musttail
tail-call-return-through-test.o: tail-call-return-through-test.c
	$(CLANG) -target x86_64-unknown-linux-gnu -c -o $@ $^

tail-call-return-through-test.exe: tail-call-return-through-test.o
	$(CC) -shared -o $@ $^

clean:
	rm ./*.exe && rm ./*.o


all: dist-test.exe tail-call-return-through-test.exe

.PHONY: clean all
