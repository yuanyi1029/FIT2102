import "./style.css";

import { fromEvent, interval } from "rxjs";
import { map, filter, scan, mergeWith } from "rxjs/operators";
import { Key, State } from "./types";
import { Constants } from "./constants";
import { render } from "./view";
import { initialState, Action, MoveX, MoveY, Tick, Rotate, Restart } from "./state";

/**
 * Core function that updates game state. Executes apply() function of each Event class
 * @param s current State
 * @param action an Event that implements Action
 * @returns an updated State
 */
const reduceState = (s: State, action: Action): State => action.apply(s);

export function main() {
  // Keypress Observable used to create more specific Observables for different keys
  const key$ = fromEvent<KeyboardEvent>(document, "keypress");

  /**
  * Function used to create Observables
  * @param keyCode key code of a keyboard key
  * @param result function that returns an Object
  * @returns a keypress Observable
  */
  const fromKey = <T>(keyCode: Key, result: () => T) =>
    key$.pipe(
      filter(({ code }) => code === keyCode),
      map(result)
    )
  
  /**
  * User Input based Observables from Keyboard Events that instantiates
  * Event classes which are subscribed to update the State (reduceState())
  */
  const left$ = fromKey("KeyA", () => new MoveX(-1));
  const right$ = fromKey("KeyD", () => new MoveX(1));
  const down$ = fromKey("KeyS", () => new MoveY(-1));
  const rotate$ = fromKey("KeyW", () => new Rotate());
  const restart$ = fromKey("KeyT", () => new Restart());

  /**
  * Main game observable that fire at intervals of 33.34 (30 frames per second). 
  * creates either a single Tick Events or other Events based on user input. Game
  * State is updated here from its initial State, and passed to render to render 
  * game graphics
  */
  const tick$ = interval(Constants.TICK_RATE_MS)
  .pipe(
    map(elapsed => new Tick(elapsed)),
    mergeWith(left$, right$, down$, rotate$, restart$),
    scan(reduceState, initialState))
  .subscribe(render)
}

if (typeof window !== "undefined") {
  window.onload = () => {
    main();
  };
}
