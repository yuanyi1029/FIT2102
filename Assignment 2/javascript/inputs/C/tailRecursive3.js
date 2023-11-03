function fibonacci2(n) {
  if (((n < 0) || (n === 0))) { return 0; }
  if ((n === 1)) { return 1; }
  return (fibonacci2((n - 1)) + fibonacci2((n - 2)));
}