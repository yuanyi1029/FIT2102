function factorial(n, acc) {
    if (((n < 0) || (n === 0))) { return acc; }
    return factorial((n - 1), (acc * n));
  }
  