main:
	ocamlbuild -pkgs unix,yojson main.byte

clean:
	ocamlbuild -clean

play:
	ocamlbuild -pkgs unix,yojson main.byte; ./main.byte