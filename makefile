BCC = ocamlc
NCC = ocamlopt

DPATH = doc/
LPATH = lib/
OPATH = obj/
BPATH = bin/
IPATH = interfaces/
SPATH = src/

IFLAGS = -I $(IPATH) -I $(LPATH)

vpath %.ml $(SPATH)
vpath %.mli $(IPATH)
vpath %.cmo $(OPATH)
vpath %.cmx $(OPATH)
vpath %.cma $(LPATH)
vpath %.cmxa $(LPATH)

schemas.cma : schemas.cmo

# Interfaces
%.mli : %.ml | interfaces
	$(BCC) -i $< > $(IPATH)$@

# Bytecode
%.cmo : %.ml | obj
	$(BCC) $(IFLAGS) -c -o $(OPATH)$@ $<

%.cma : %.cmo | lib
	$(BCC) $(IFLAGS) -a -o $(LPATH)$@ $(OPATH)$<
	# ?!
	cp $(OPATH)*.cmi $(LPATH)

# Native code
%.cmx : %.ml | obj
	$(NCC) $(IFLAGS) -c -o $(OPATH)$@ $<

%.cmxa : %.cmx | lib
	$(NCC) $(IFLAGS) -a -o ($LPATH)$@ $(OPATH)$<

lib :
	mkdir -p $(LPATH)

obj :
	mkdir -p $(OPATH)

bin :
	mkdir -p $(BPATH)

interfaces :
	mkdir -p $(IPATH)

doc :
	mkdir -p $(DPATH)

clean :
	rm -rf $(BPATH)* $(OPATH)*

cleanall : clean
	rm -rf $(LPATH)* $(DPATH)*
