EXE_FILE = ndate

$(EXE_FILE):  ndate.f
	$(FCMP) -o $(EXE_FILE) ndate.f $(W3NCO_LIB4)

install:
	mkdir -p ../../exec
	cp -p $(EXE_FILE) ../../exec

clean:
	-rm -f $(EXE_FILE)

