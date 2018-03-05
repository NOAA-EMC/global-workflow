EXE_FILE = nhour

$(EXE_FILE):  nhour.f
	$(FCMP) -o $(EXE_FILE) nhour.f $(W3NCO_LIB4)

install:
	mkdir -p ../../exec
	cp -p $(EXE_FILE) ../../exec

clean:
	-rm -f $(EXE_FILE)

