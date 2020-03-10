export void scan_ispc(uniform float output[], uniform float input[], uniform int n) {
	if (programIndex < programCount) {
		float sum1 = input[programIndex];
		output[programIndex] = exclusive_scan_add(sum1) + input[programIndex];
	}
	for (int i = programCount + programIndex; i < n; i += programCount) {
		float sum = input[i];
		int prev = i / programCount * programCount - 1;
		float value = output[prev];
		output[i] = exclusive_scan_add(sum) + input[i] + value;
	}
}
