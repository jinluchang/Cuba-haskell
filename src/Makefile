name = program

hc = ghc -Wall -O2
main = Main.hs
source = Main.hs Cuba.hs

flags = -ICuba-2.1
libs = -lm -lcuba -LCuba-2.1

clean = $(name) *.hi *.o *_stub.*

all : $(name)

run : $(name)
	./$(name)

$(name) : $(source)
	$(hc) --make -o $@ $(main) $(flags) $(libs)

clean :
	rm $(clean)

