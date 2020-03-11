export uniform int pack_ispc(uniform int output[], uniform int input[], uniform int n) {
  uniform int m = 0;
  foreach (i = 0 ... n) {
    float j = input[i];
    int keep = j != input[i-1]; 
    int offset = exclusive_scan_add(keep);
    if (!keep) {
      offset = programCount-1;
    }
    output[m + offset] = j;
    m += reduce_add(keep);
  }
  return m;
}
