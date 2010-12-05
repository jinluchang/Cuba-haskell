all : main

main : *.hs
	ghc -O2 -Wall -o main --make Main.hs -ICuba-2.1 -LCuba-2.1 -lcuba

clean :
	rm *.hi *.o *.h *.c

dist-clean :
	rm main
