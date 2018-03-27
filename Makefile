all: surface4d

.PHONY:surface4d
surface4d:
	jbuilder build src/surface4d/surface4d.exe
	_build/default/src/surface4d/surface4d.exe

.PHONY:clean
clean:
	jbuilder clean

install:
	jbuilder build @install
	jbuilder install
