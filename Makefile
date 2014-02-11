CASK ?= cask

all: win

win:
	${CASK} exec ecukes --reporter spec --win features

nowin:
	${CASK} exec ecukes --reporter spec --no-win features

cask:
	${CASK} install

travis: cask win
