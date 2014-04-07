SRC=  WavWriter.ml          \
      NoteMaker.ml          \
      main.ml

TARGET= ocamlfm

$(TARGET):
	cd src; ocamlc $(SRC) -o ../$(TARGET)

clean:
	rm -rf src/*.cm? $(TARGET)
