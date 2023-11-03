function fibonacci(n, pprev, prev) {
    if (((n < 0) || (n === 0))) { return pprev; }
    if ((n === 1)) { return prev; }
    return fibonacci((n - 1), prev, (pprev + prev));
  }
  