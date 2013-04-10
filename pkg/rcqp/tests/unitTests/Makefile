TOP=../..
PKG=${shell cd ${TOP};pwd}
SUITE=doRUnit.R
R=R

all: test

# Install package
inst: 
	cd ${TOP}/..;\
	${R} CMD INSTALL ${PKG}

setup:
	cd ${TOP}/tests;\
	RCMDCHECK=FALSE CORPUS_REGISTRY=${PKG}/tests/unitTests/registry ${R} --no-environ CMD BATCH setTestCorpora.R

# Run unit tests
test: 
	cd ${TOP}/tests;\
	RCMDCHECK=FALSE CORPUS_REGISTRY=${PKG}/tests/unitTests/registry ${R} --no-environ CMD BATCH ${SUITE}
