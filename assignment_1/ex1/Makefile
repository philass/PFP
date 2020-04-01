two_%_i32s:
	futhark dataset -b --i32-bounds=-10000:10000 \
                  -g [$*]i32 -g [$*]i32 > $@

SIZES = 100  1000 10000 100000 1000000 5000000 10000000


ex1: exercise_1_1-opencl.json exercise_1_1-c.json exercise_1_3-opencl.json exercise_1_3-c.json

ex1_1: exercise_1_1-opencl.json exercise_1_1-c.json 

ex2_3: exercise_1_3-opencl.json exercise_1_3-c.json
 
exercise_1_1-opencl.json : $(SIZES:%=two_%_i32s)  ex1_1.fut
	futhark bench --backend=opencl \
                --json=exercise_1_1-opencl.json \
                ex1_1.fut

exercise_1_1-c.json : $(SIZES:%=two_%_i32s)  ex1_1.fut
	futhark bench --backend=c \
                --json=exercise_1_1-c.json \
                ex1_1.fut


exercise_1_3-opencl.json : $(SIZES:%=two_%_i32s)  ex1_3.fut
	futhark bench --backend=opencl \
                --json=exercise_1_3-opencl.json \
                ex1_3.fut

exercise_1_3-c.json : $(SIZES:%=two_%_i32s)  ex1_3.fut
	futhark bench --backend=c \
                --json=exercise_1_3-c.json \
                ex1_3.fut


clean:
	rm *i32s *json rm *.c
