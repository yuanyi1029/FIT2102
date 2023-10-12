// https://www.freecodecamp.org/news/build-a-wordle-clone-in-javascript/
// Web page adapated from here

import { ajax } from "rxjs/ajax";
import { map, filter, takeWhile, concatMap, take } from "rxjs/operators";
import { fromEvent, merge, mergeScan, type Observable, of } from "rxjs";
import toastr from "toastr";

const NUMBER_OF_GUESSES = 6;

type State = Readonly<{
  currentGuesses: number;
  targetWord: string;
  currentGuess: string;
  previousGuess: string;
  pressedKey: string | null;
  complete: boolean;
  success: boolean;
  enter: boolean;
  feedback: string[] | null;
  validGuess: string | null;
  error: string | null;
}>;

function initBoard(): void {
  const board = document.getElementById("game-board");

  if (board === null) {
    return;
  }

  Array.from({ length: NUMBER_OF_GUESSES }).forEach(() => {
    const row = document.createElement("div");
    row.className = "letter-row";

    Array.from({ length: 5 }).forEach(() => {
      const box = document.createElement("div");
      box.className = "letter-box";
      row.appendChild(box);
    });

    board.appendChild(row);
  });
}

function wordCreator(): Observable<string> {
  return ajax({
    url: "/api/getTarget",
    method: "POST",
    headers: {
      "Content-Type": "application/x-www-form-urlencoded",
    },
    body: "",
  }).pipe(
    map((response) => response.response), // Extracting the response data
    map((data: any) => {
      return data.target;
    })
  );
}

function shadeKeyBoard(letter, color): void {
  const elements = Array.from(
    document.getElementsByClassName("keyboard-button")
  );

  const targetElement = elements.find(
    (elem) => elem.textContent === letter
  ) as HTMLElement;

  if (targetElement !== null) {
    const oldColor = targetElement.style.backgroundColor;

    if (oldColor === "green" || (oldColor === "yellow" && color !== "green")) {
      return;
    }
    targetElement.style.backgroundColor = color;
  }
}

const animateCSS = async (element, animation, prefix = "animate__") =>
  // We create a Promise and return it
  await new Promise((resolve, reject) => {
    const animationName = `${prefix}${animation}`;
    // const node = document.querySelector(element);
    const node = element;
    node.style.setProperty("--animate-duration", "0.3s");

    node.classList.add(`${prefix}animated`, animationName);

    // When the animation ends, we clean the classes and resolve the Promise
    function handleAnimationEnd(event): void {
      event.stopPropagation();
      node.classList.remove(`${prefix}animated`, animationName);
      resolve("Animation ended");
    }

    node.addEventListener("animationend", handleAnimationEnd, { once: true });
  });

interface Action {
  apply: (s: State) => State;
}

class Insert implements Action {
  constructor(public readonly letter: string) {}
  apply(s: State): State {
    if (s.currentGuess.length === 5) {
      return s;
    }
    return {
      ...s,
      currentGuess: s.currentGuess + this.letter,
      pressedKey: this.letter,
    };
  }
}

class Enter implements Action {
  constructor() {}
  apply(s: State): State {
    return { ...s, enter: true };
  }
}

function EnterCreator(s: State): Observable<State> {
  if (s.currentGuess.length !== 5) {
    return of({ ...s, error: "Not enough letters!" });
  }
  return ajax({
    url: "/api/makeGuess",
    method: "POST",
    headers: {
      "Content-Type": "application/x-www-form-urlencoded",
    },
    body: JSON.stringify({ guess: s.currentGuess, target: s.targetWord, previousGuess: s.previousGuess }),
  }).pipe(
    map((response) => response.response), // Extracting the response data
    map((data: any): State => {
      if (data.valid === "False") {
        return { ...s, error: "Invalid Guess" };
      }
      const correctGuess = s.targetWord === s.currentGuess;
      return {
        ...s,
        enter: false,
        feedback: JSON.parse(data.feedback),
        success: correctGuess,
        complete: correctGuess || s.currentGuesses === 5,
        currentGuesses: s.currentGuesses + 1,
        previousGuess: s.currentGuess,
        currentGuess: "",
      };
    })
  );
}

class Delete implements Action {
  apply(s: State): State {
    return { ...s, currentGuess: s.currentGuess.slice(0, -1) };
  }
}

class CreateWord implements Action {
  constructor(public readonly word: string) {}
  apply(s: State): State {
    return { ...s, targetWord: this.word };
  }
}

function main() {
  const keyboardCont = document.getElementById("keyboard-cont")!;

  const keyboardMouse$ = fromEvent(keyboardCont, "click").pipe(
    filter((e) => {
      const target = e.target as HTMLElement;
      return target.classList.contains("keyboard-button");
    }),
    map((e) => {
      const target = e.target as HTMLElement;
      const tc = target.textContent;
      const key = tc === "Del" ? "Backspace" : (tc as string | undefined);
      return new KeyboardEvent("keyup", { key });
    })
  );

  const keyup$ = merge(keyboardMouse$, fromEvent(document, "keyup"));

  const delete$: Observable<Action> = keyup$.pipe(
    filter((e) => {
      const pressedKey = String((e as KeyboardEvent).key);
      return pressedKey === "Backspace";
    }),
    map(() => new Delete())
  );

  const enter$: Observable<Action> = keyup$.pipe(
    filter((e) => String((e as KeyboardEvent).key) === "Enter"),
    map(() => new Enter())
  );

  // insertLetter(String((e as KeyboardEvent).key))
  const letters$: Observable<Action> = keyup$.pipe(
    filter((e) => /^[a-z]$/.test(String((e as KeyboardEvent).key))),
    map((e) => new Insert(String((e as KeyboardEvent).key)))
  );

  const initial_state: State = {
    currentGuesses: 0,
    pressedKey: null,
    targetWord: "press",
    currentGuess: "",
    complete: false,
    success: false,
    enter: false,
    feedback: null,
    validGuess: null,
    error: null,
    previousGuess: "",
  };

  const reduceState = (s: State, action: Action) => action.apply(s);
  const nullify = (s: State): State => ({
    ...s,
    pressedKey: null,
    enter: false,
    feedback: null,
    validGuess: null,
    error: null,
  });

  // wordCreator() Observable<String> which will only fire once and give the targetWord to the state
  wordCreator()
    .pipe(
      take(1),
      map((x) => new CreateWord(x)),
      concatMap((targetWord) =>
        merge(of(targetWord), delete$, enter$, letters$)
      )
    )
    .pipe(
      mergeScan((acc, value) => {
        if (value instanceof Enter) {
          return EnterCreator(nullify(acc));
        } else {
          return of(reduceState(nullify(acc), value));
        }
      }, initial_state),
      takeWhile((s) => !s.complete, true)
    )
    .subscribe(render);

  function render(state: State) {
    if (state.currentGuesses <= 5) {
      const row =
        document.getElementsByClassName("letter-row")[state.currentGuesses];
      Array.from(row.children).forEach((box, i) => {
        if (i >= state.currentGuess.length) {
          box.textContent = "";
          box.classList.remove("filled-box");
        } else {
          if (!box.classList.contains("filled-box")) {
            animateCSS(box, "pulse");
          }
          box.textContent = state.currentGuess[i];
          box.classList.add("filled-box");
        }
      });
    }
    if (state.complete) {
      if (state.success) {
        toastr.success("You guessed right! Game over!");
      } else {
        toastr.error("You've run out of guesses! Game over!");
        toastr.info(`The right word was: "${state.targetWord}"`);
      }
    }
    if (state.feedback !== null) {
      const prevRow =
        document.getElementsByClassName("letter-row")[state.currentGuesses - 1];

      Array.from(prevRow.children).forEach((box, i) => {
        const delay = 250 * i;

        setTimeout(() => {
          // Flip box
          animateCSS(box, "flipInX");
          // Shade box
          if (state.feedback !== null) {
            (box as HTMLElement).style.backgroundColor = state.feedback[i];
            shadeKeyBoard(state.previousGuess.charAt(i), state.feedback[i]);
          }
        }, delay);
      });
    }
    if (state.error !== null) {
      toastr.error(state.error);
    }
  }
}

// The following simply runs your main function on window load.  Make sure to leave it in place.
if (typeof window !== "undefined") {
  window.onload = () => {
    initBoard();
    main();
  };
}
