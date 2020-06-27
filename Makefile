
c_binding_extras.hf :c_binding_extras.c
	cc c_binding_extras.c -o c_binding_extras
	./c_binding_extras >c_binding_extras.hf
	rm -f c_binding_extras
	touch c_binding_extras.c
