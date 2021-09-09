
IMAGES=philadelphia.svg brokkoli.svg

SIZE=64x64

all: $(IMAGES:.svg=.png)

%.png: %.svg
	convert -size $(SIZE) $< $@ 

clean:
	rm $(IMAGES:.svg=.png)

