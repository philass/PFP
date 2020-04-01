two_%_i32s:
	futhark dataset -b --i32-bounds=-10000:10000 \
                  -g [$*]i32 > $@

SIZES = 100  1000 10000 100000 1000000 5000000 10000000



ex2_1: exercise_2_1-opencl.json exercise_2_1-c.json 

 
exercise_2_1-opencl.json : $(SIZES:%=two_%_i32s)  hillis-steele.fut
	futhark bench --backend=opencl \
                --json=exercise_2_1-opencl.json \
                hillis-steele.fut

exercise_2_1-c.json : $(SIZES:%=two_%_i32s)  hillis-steele.fut
	futhark bench --backend=c \
                --json=exercise_2_1-c.json \
                hillis-steele.fut



clean:
	rm *i32s *json rm *.c
