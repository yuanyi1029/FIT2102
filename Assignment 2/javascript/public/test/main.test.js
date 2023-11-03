describe("exercise_2_suite", function () {
  describe("(const1.js) a2_3aBcD1 exists", function () {
    it("Create an un-reassignable variable called a2_3aBcD1 and initialise its value to 1.", function () {
      expect(a2_3aBcD1).to.equal(1);
    });
    it("Since a2_3aBcD1 is immutable it will throw an error at runtime if I try to change it.", function () {
      try {
        a2_3aBcD1 = 3;
      } catch (e) {
        console.log(e.message);
        expect(e.name).to.equal("TypeError");
      }
      expect(a2_3aBcD1).to.equal(1);
    });
  });
  describe("(const1.js) b2 exists", function () {
    it("Another un-reassignable b2", function () {
      expect(b2).to.equal(2);
    });
    it("Since b2 is immutable it will throw an error at runtime if I try to change it.", function () {
      try {
        b2 = 3;
      } catch (e) {
        console.log(e.message);
        expect(e.name).to.equal("TypeError");
      }
      expect(b2).to.equal(2);
    });
  });

  describe("(const2.js) list exists", function () {
    it("Create an un-reassignable variable called list", function () {
      expect(list.length).to.equal(3);
    });
    it("list should have correct values", function () {
      expect(list).to.deep.equal([2, 3, 4]);
    });
  });
});

describe("exercise_3_suite", function () {
  describe("(if1.js) if1 exists", function () {
    it("Create an un-reassignable variable after the block.", function () {
      expect(if1).to.equal(12);
    });
  });
  describe("(if2.js) if2 exists", function () {
    it("Create an un-reassignable variable after the block.", function () {
      expect(if2).to.equal(1);
    });
  });
});

describe("exercise_4_suite", function () {
  describe("(funccall1.js) fc1 exists", function () {
    it("Test output of a function call with a single parameter", function () {
      expect(fc1).to.equal(1234);
    });
  });
  describe("(funccall2.js) fc2 exists", function () {
    it("Test output of a function call with multiple parameters", function () {
      expect(fc2).to.equal(12);
    });
  });
});

describe("exercise_5_suite", function () {
  describe("(funcdef1.js) someNumber exists", function () {
    it("Test function call with no parameter", function () {
      expect(someNumber()).to.equal(124);
    });
  });
  describe("(funcdef2.js) someNumber2 exists", function () {
    it("Test function call with multiple parameters", function () {
      expect(someNumber2(3, 5)).to.equal(129);
    });
  });
});

describe("exercise_7_suite", function () {
  describe("Tail Recursive checks", function () {
    it("The results of checking the three files must be correct boolean", function () {
      expect(tailRecursiveResults).to.deep.equal(['True', 'True', 'False']);
    });
  });
});


describe("exercise_8_suite", function () {
  describe("(tailRecursive1.js) factorial exists", function () {
    it("Test factorial", function () {
      expect(factorial(5, 1)).to.equal(120);
    });
  });
  describe("(tailRecursive2.js) fibonacci exists", function () {
    it("Test fibonacci", function () {
      expect(fibonacci(10, 0, 1)).to.equal(55);
    });
  });
  describe("(tailRecursive3.js) fibonacci exists", function () {
    it("Test fibonacci", function () {
      expect(fibonacci2(10)).to.equal(55);
    });
  });
});
