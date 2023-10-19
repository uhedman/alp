TESTDIRS += tests/ok/00-basicos
TESTDIRS += tests/ok/10-sugar
TESTDIRS += tests/ok/20-tysym

TESTS	:= $(shell find $(TESTDIRS) -name '*.fd4' -type f | sort)

# Los binarios. La primer línea es una magia para encontrar el
# ejecutable que crea stack, porque correr 'stack run' es recontra lento
# (~30x).
EXE	:= $(shell cabal exec whereis prooftheorem | awk '{print $$2};')

EXTRAFLAGS	:=

# Las reglas a chequear. Se puede deshabilitar toda una familia de tests
# comentando una de estas líneas.
CHECK	+= $(patsubst %,%.check_eval,$(TESTS))

# Ejemplo: así se puede apagar un test en particular.
# CHECK	:= $(filter-out tests/correctos/grande.fd4.check_bc32,$(CHECK))

# Esta regla corre todos los tests (por sus dependencias) y luego
# imprime un mensaje.
test_all: $(CHECK)
	@echo "---------------------------------"
	@echo "             Todo OK             "
	@echo "---------------------------------"

Q=@
ifneq ($(V),)
	Q=
endif

# Esto cancela la regla por defecto de make para generar un .out
# copiando el archivo original.
%.out: %

# Aceptar la salida de los programas como correcta. Copia de la salida
# real del evaluador hacia los .out que contienen la salida esperada.
#
# Si no existen los archivos, los crea, así que esto puede usarse para
# agregar un nuevo test.
#
# La _única_ salida que se acepta es la del --eval. Todos los demás
# evaluadores/backends deben coincidir.
accept: $(patsubst %,%.accept,$(TESTS))

# La otra salida esperada es la de las optimizaciones.
# accept: $(patsubst %,%.accept_opt,$(TESTS))

%.accept: %.actual_out_eval
	@echo "ACCEPT	$(patsubst %.accept,%,$@)"
	$(Q)cp $< $(patsubst %.actual_out_eval,%.out,$<)

# Generar salida con el evaluador naive.
%.actual_out_eval: % $(EXE)
	$(Q)$(EXE) $(EXTRAFLAGS) --eval $< > $@

# Comparar salida naive con esperada.
%.check_eval: %.out %.actual_out_eval
	$(Q)diff -u $^
	$(Q)touch $@
	@echo "OK	EVAL	$(patsubst %.out,%,$<)"

# Estas directivas indican que NO se borren los archivos intermedios,
# así podemos examinarlos, particularmente cuando algo no anda.
.SECONDARY: $(patsubst %,%.actual_out_eval,$(TESTS))
.SECONDARY: $(patsubst %,%.actual_opt_out,$(TESTS))

.PHONY: test_all accept
