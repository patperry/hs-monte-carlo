all:
	ghc -O -package ieee754 -i. -i../lib Main.hs --make -o test-mc
	./test-mc

hpc:
	ghc -fforce-recomp -package ieee754 \
	    -i. -i../lib -fhpc --make Main.hs -o test-mc
	rm -f test-mc.tix
	./test-mc
	hpc markup test-mc

clean:
	find ../lib . -name '*.hi' | xargs rm -f
	find ../lib . -name '*.o'  | xargs rm -f
	find . -name '*.html' | xargs rm -f
	rm -f test-mc test-mc.tix
	rm -rf .hpc
	