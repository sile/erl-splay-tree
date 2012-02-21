all:
	mkdir -p ebin
	erl -make

clean:
	rm ebin/*
