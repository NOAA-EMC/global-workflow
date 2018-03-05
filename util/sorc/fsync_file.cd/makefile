EXE_FILE = fsync_file

$(EXE_FILE):  fsync_file.c
	$(CCMP) -o fsync_file fsync_file.c

install:
	mkdir -p ../../exec
	cp -p $(EXE_FILE) ../../exec

clean:
	-rm -f $(EXE_FILE)

