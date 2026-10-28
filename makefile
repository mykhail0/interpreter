BNFC = bnfc
BNFC_OPTS = --haskell --functor -m

SRC_DIR = app

all: interpreter

${SRC_DIR}/AbsProgram.hs: ${SRC_DIR}/Program.cf
	${BNFC} ${BNFC_OPTS} $< -o ${SRC_DIR}

interpreter: ${SRC_DIR}/Main.hs ${SRC_DIR}/Interpreter.hs ${SRC_DIR}/AbsProgram.hs ${SRC_DIR}/LexProgram.hs ${SRC_DIR}/ParProgram.hs ${SRC_DIR}/ErrM.hs
	cd ${SRC_DIR} && make GHC=/home/mykhail0/.ghcup/bin/ghc
