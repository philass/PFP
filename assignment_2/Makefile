results: make_data_files matrix_benchmark.json


DATAFILES = data_1024_32 data_128_2048 data_16_131072 data_256_512 data_32_32768 data_512_128 data_64_8192

make_data_files: dataset.py
	python3 dataset.py

matrix_benchmark.json: $(DATAFILES) matrix-inversion.fut
	futhark bench --backend=opencl --json=matrix_benchmark.json matrix-inversion.fut

clean:
	rm data_* && rm matrix_benchmark.json && rm ./matrix-inversion && rm *.c

