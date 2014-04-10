SRC=  WavWriter.ml          \
      Oscillators.ml        \
      NoteMaker.ml          \
      MelodyMaker.ml        \
      main.ml

TARGET= ocamlfm

$(TARGET): $(addprefix src/, $(SRC))
	cd src; ocamlc $(SRC) -o ../$(TARGET)

clean:
	rm -rf src/*.cm? $(TARGET)
