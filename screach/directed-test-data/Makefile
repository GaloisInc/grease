DOCKER_IMAGE := test-gen
ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))


.PHONY: clean

all: \
	avoid-addr.tsv \
	avoid-addr.elf \
	do-not-return-to-same-block.elf \
	do-not-return-to-same-block.tsv

%.elf: %.c
	docker run  \
		-u "$(id -u)" \
		-v ${ROOT_DIR}:/out \
		${DOCKER_IMAGE} \
		/root/x-tools/x86_64-unknown-linux-gnu/bin/x86_64-unknown-linux-gnu-gcc /out/$< -o /out/$@

%.tsv: %.elf
	docker run \
		-u "$(id -u)" \
		-v ${ROOT_DIR}:/out \
		${DOCKER_IMAGE} \
		python /callgraph-script/main.py --entrypoint main /out/$< /out/$@

clean:
	rm -f *.elf *.tsv
