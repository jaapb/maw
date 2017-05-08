.SUFFIXES: .ml .cmi .cmo .cmx

LINK_PKG=	-package csv,pgocaml,cryptokit,calendar
COMP_PKG=	-package csv,pgocaml,pgocaml.syntax,cryptokit,calendar
CFLAGS=	-syntax camlp4o

cronjob: cronjob.cmx
	ocamlfind ocamlopt -I ../_server ${LINK_PKG} -package netclient,nettls-gnutls -linkpkg -o $@ mail.cmx $>

fill_database_users: fill_database_users.cmx
	ocamlfind ocamlopt ${LINK_PKG} -linkpkg -o $@ $>

fill_database_games: fill_database_games.cmx
	ocamlfind ocamlopt ${LINK_PKG} -linkpkg -o $@ $>

fill_database_bookings: fill_database_bookings.cmx
	ocamlfind ocamlopt ${LINK_PKG} -linkpkg -o $@ $>

.mli.cmi:
	ocamlfind ocamlc ${COMP_PKG} ${CFLAGS} -I ../_server -o $@ -c $<

.ml.cmo:
	ocamlfind ocamlc ${COMP_PKG} ${CFLAGS} -I ../_server -o $@ -c $<

.ml.cmx:
	ocamlfind ocamlopt ${COMP_PKG} ${CFLAGS} -I ../_server -o $@ -c $<

