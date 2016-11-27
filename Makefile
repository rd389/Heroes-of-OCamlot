main:
	ocamlbuild -pkgs oUnit,yojson,ANSITerminal main.byte

clean:
	ocamlbuild -clean

play:
	ocamlbuild -pkgs oUnit,yojson,ANSITerminal main.byte; ./main.byte