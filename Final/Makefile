default: compile

HELP = final_helpers
NAME = final
INT  = final_interactive

default: compile
all: compile interactive test

compile: clean
	ocamlfind ocamlc -c ${HELP}.mli ${HELP}.ml
	ocamlfind ocamlc -c ${NAME}.mli ${NAME}.ml

interactive: compile
	ocamlfind ocamlc -o ${INT} \
	  -package str -linkpkg \
	  ${HELP}.cmo ${NAME}.cmo ${INT}.ml
	./${INT}

test: compile
	ocamlfind ocamlc -o tests_${NAME} \
	  -package oUnit -linkpkg \
	  ${HELP}.cmo ${NAME}.cmo tests_${NAME}.ml
	./tests_${NAME}

clean:
	rm -f *.cm* *.log *.cache tests_${NAME} ${INT}
