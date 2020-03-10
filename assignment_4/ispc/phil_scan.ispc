export void scan_ispc(uniform float output[], uniform float input[], uniform int n) {
	int sum = 0;
	foreach (i = 0 ... n) {
		sum += input[i];
		int prev = i / programCount * programCount - 1;
		float value = prev >= programCount - 1 ? output[prev] : 0;
		output[i] = exclusive_scan_add(sum) ;//+ input[i] ;//+ value;
	}
}
