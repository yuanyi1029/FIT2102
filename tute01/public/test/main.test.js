describe("exercise_1_suite", function () {
  describe("firstConst", function () {
    it("Create an un-reassignable variable called firstConst and initialise its value to 1.", function () {
      expect(firstConst).to.equal(1);
    });
    it("Since firstConst is immutable it will throw an error at runtime if I try to change it.", function () {
      try {
        firstConst = 3;
      } catch (e) {
        console.log(e.message);
        expect(e.name).to.equal("TypeError");
      }
      expect(firstConst).to.equal(1);
    });
  });
  describe("secondConst", function () {
    it("Then create another un-reassignable variable called secondConst and initialise its value to secondConst + 1.", function () {
      expect(secondConst).to.equal(firstConst + 1);
    });
    it("Since secondConst is immutable it will throw an error at runtime if I try to change it.", function () {
      try {
        secondConst = 3;
      } catch (e) {
        console.log(e.message);
        expect(e.name).to.equal("TypeError");
      }
      expect(secondConst).to.equal(firstConst + 1);
    });
  });
});

describe("exercise_2_suite", function () {
  describe("aFunction", function () {
    it("aFunction exists and is a function", function () {
      expect(aFunction).is.a("function");
    });
    it("invoking aFunction should return the value of 4", function () {
      expect(aFunction()).to.equal(4);
    });
  });
  describe("anonymousFunction", function () {
    it("anonymousFunction exists and is a function", function () {
      expect(anonymousFunction).is.a("function");
    });
    it("invoking anonymousFunction should return the value of 4", function () {
      expect(anonymousFunction()).to.equal(4);
    });
  });
  describe("arrowFunction", function () {
    it("arrowFunction exists and is a function", function () {
      expect(arrowFunction).is.a("function");
    });
    it("invoking arrowFunction should return the value of 4", function () {
      expect(arrowFunction()).to.equal(4);
    });
  });
});

describe("exercise_3_suite", function () {
  describe("Implement helper functions", function () {
    describe("`isDivisibleByThreeOrFive`", function () {
      it("exists", function () {
        expect(isDivisibleByThreeOrFive).is.a("function");
      });
      it("is correct for input 1", function () {
        expect(isDivisibleByThreeOrFive(1)).to.be.false;
      });
      it("is correct for input 3", function () {
        expect(isDivisibleByThreeOrFive(3)).to.be.true;
      });
      it("is correct for input 5", function () {
        expect(isDivisibleByThreeOrFive(5)).to.be.true;
      });
      it("is correct for input 15", function () {
        expect(isDivisibleByThreeOrFive(15)).to.be.true;
      });
      it("is correct for input 16", function () {
        expect(isDivisibleByThreeOrFive(16)).to.be.false;
      });
    });    
    describe("`selectiveSummer`", function () {
      it("exists", function () {
        expect(selectiveSummer).is.a("function");
      });
      it("sums valid numbers up to the number given", function () {
        expect(selectiveSummer((x) => x != 10)(10)).to.equal(45);
      });
    });
    describe("`filteredSum`", function () {
      it("exists", function () {
        expect(filteredSum).is.a("function");
      });
      it("calculates Project Euler Problem 1 sums", function () {
        expect(filteredSum(10)).to.equal(23);
      });
    });
  });

  describe("Project Euler Problem 1 function `projectEulerProblem1`", function () {
    it("exists", function () {
      expect(projectEulerProblem1).is.a("function");
    });
    it("produces the correct value", function () {
      const r = projectEulerProblem1();
      expect(hash(r + "")).to.equal(String(166417277));
    });
  });
});

describe("exercise_4_suite", function () {
  describe("Function `printArray`", function () {
    it("exists", function () {
      expect(printArray).is.a("function");
    });
    it("returns no output", function () {
      expect(printArray([])).to.be.undefined;
      expect(printArray([1, 2, 3])).to.be.undefined;
      expect(printArray([5])).to.be.undefined;
    });
  });

  describe("Function `addOne`", function () {
    it("exists", function () {
      expect(addOne).is.a("function");
    });
    it("adds one to the input", function () {
      const data = [[1, 2, 3], [1], [], [-1, 0, 1], [5, 5, 5, 5, 5, 5, 5]];
      expect(data.map(addOne)).to.deep.equal([
        [2, 3, 4],
        [2],
        [],
        [0, 1, 2],
        [6, 6, 6, 6, 6, 6, 6],
      ]);
    });
  });

  describe("Function `removeOnes`", function () {
    it("exists", function () {
      expect(removeOnes).is.a("function");
    });
    it("removes ones", function () {
      expect(removeOnes([1, 2, 3, 1, 1, 2])).to.deep.equal([2, 3, 2]);
    });
    it("ignores non-ones", function () {
      expect(removeOnes([2, 3, 4, 5])).to.deep.equal([2, 3, 4, 5]);
    });
  });

  describe("Function `sumArray`", function () {
    it("exists", function () {
      expect(sumArray).is.a("function");
    });
    it("sums arrays", function () {
      const data = [[1, 2, 3], [1], [], [-1, 0, 1], [5, 5, 5, 5, 5, 5, 5]];
      expect(data.map(sumArray)).to.deep.equal([6, 1, 0, 0, 35]);
    });
  });
});

describe("exercise_5_suite", function () {
  describe("Function `range`", function () {
    it("exists", function () {
      expect(range).is.a("function");
    });
    it("produces the correct array", function () {
      const r = range(5)
      expect(r).to.eql([0,1,2,3,4]);    
    });
    it("handles the zero case", function () {
      const r = range(0)
      expect(r).to.eql([]);    
    });
  });

  describe("Project Euler Problem 1 function `projectEulerProblem1Again`", function () {
    it("exists", function () {
      expect(projectEulerProblem1Again).is.a("function");
    });
    it("produces the correct value", function () {
      const r = projectEulerProblem1Again();
      expect(hash(r + "")).to.equal(String(166417277));
    });
  });
});

describe("exercise_6_suite", function () {
  const list = () => cons(1, cons(2, cons(3, cons(4, cons(5, null)))));
  const f = (h, r, acc = []) => {
    const newAcc = acc.concat(h);
    if (r === null) {
      return newAcc;
    }
    return newAcc.concat(r(f));
  };

  describe("function `cons`", function () {
    it("exists", function () {
      expect(cons).is.a("function");
    });
    it("creates a list", function () {
      expect(list).to.not.throw();
      const data = list()(f);
      expect(data).to.deep.equal([1, 2, 3, 4, 5]);
    });
    it("stores values correctly", function () {
      expect(
        cons(1, null),
        "expected cons(1, undefined) to return a selector function"
      ).is.a("function");
      cons(
        1,
        2
      )((head, rest) => {
        expect(
          head,
          "cons isn't properly storing the head in the closure"
        ).to.equal(1);
        expect(
          rest,
          "cons isn't properly storing the rest in the closure"
        ).to.equal(2);
      });
    });
  });

  describe("function `head`", function () {
    it("exists", function () {
      expect(head).is.a("function");
    });
    expect("extracts the value of the cons list", function () {
      expect(head(list())).to.equal(1);
    });
  });

  describe("function `rest`", function () {
    it("exists", function () {
      expect(rest).is.a("function");
    });
    it("extracts the rest of the cons list", function () {
      expect(rest(cons(1, null))).is.equal(null);
      expect(
        head(rest(list())),
        "Expected head of list returned by rest to be 2"
      ).to.equal(2);
      expect(head(rest(rest(list())))).to.equal(3);
    });
  });
});

describe("exercise_7_suite", function () {
  const list = () => cons(1, cons(2, cons(3, cons(4, cons(5, null)))));
  const f = (h, r, acc = []) => {
    const newAcc = acc.concat(h);
    if (r === null) {
      return newAcc;
    }
    return newAcc.concat(r(f));
  };

  describe("function `map`", function () {
    it("exists", function () {
      expect(map).is.a("function");
    });
    it("can be used to add one", function () {
      console.log(list()(f));
      expect(map((x) => x + 1, list())(f)).to.deep.equal(
        list()(f).map((x) => x + 1)
      );
    });
  });

  describe("function `reduce`", function () {
    it("exists", function () {
      expect(reduce).is.a("function");
    });
    it("can be used to calculate a sum", function () {
      expect(reduce((acc, x) => acc + x, 0, list())).to.equal(15);
    });
    it("reduces in the correct order from left to right", function () {
      expect(reduce((acc, x) => x - acc, 0, list())).to.equal(3);
    });
  });

  describe("function `filter`", function () {
    it("exists", function () {
      expect(filter).is.a("function");
    });

    it("can be used to remove odd numbers", function () {
      expect(filter((x) => x % 2 === 0, list())(f)).to.deep.equal(
        list()(f).filter((x) => x % 2 === 0)
      );
    });
  });
});
