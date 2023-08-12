import { Observable, finalize, from, interval, take } from "rxjs";
import { createRngStreamFromSource } from "../src/main.ts";

import { expect } from "chai";

describe("exercise_1_suite", function () {
  describe("createRngStreamFromSource", function () {
    it("is a curried function", function () {
      expect(createRngStreamFromSource).is.a("function");
      expect(createRngStreamFromSource(interval())).is.a("function");
    });
  });

  describe("createRngStream", function () {
    it("creates an Observable stream", function () {
      const createRngStream = createRngStreamFromSource(interval());

      expect(createRngStream).to.not.throw();
      expect(createRngStream()).to.be.an.instanceof(Observable);
    });

    it("returns a stream with values in the range [-1, 1]", function (done) {
      const inf = {
        [Symbol.iterator]: function* () {
          while (true) yield true;
        },
      };
      const infiniteStream = from(inf);

      const createRngStream = createRngStreamFromSource(infiniteStream);

      expect(createRngStream).to.not.throw();
      createRngStream()
        .pipe(take(500), finalize(done))
        .subscribe(function (val) {
          expect(val).to.be.within(-1, 1);
        });
    });
  });
});
