export void scan_ispc(uniform float output[], uniform float input[], uniform int n) {
	float sum;
	foreach (i = 0 ... n) {
		sum += input[i];
	}
	for (uniform int i = 0; i < n; i++) {
	float val = broadcast(sum, i);
	output[programIndex] = exclusive_scan_add(val);
	}

}
