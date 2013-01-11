CARTON ?= carton
ECUKES = $(shell find elpa/ecukes-*/ecukes | tail -1)

all: win

win:
	${CARTON} exec ${ECUKES} --dbg --verbose features

carton:
	${CARTON} install

travis: carton win
