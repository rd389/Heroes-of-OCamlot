main:
	ocamlbuild -pkgs oUnit,ANSITerminal main.byte

clean:
	ocamlbuild -clean