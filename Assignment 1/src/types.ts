/** Types */
import { RNG } from "./util";

export type Key = "KeyS" | "KeyA" | "KeyD" | "KeyW" | "KeyT";
export type Event = "keydown" | "keyup" | "keypress";
export type TetroType = "O" | "I" | "T" | "J" | "L" | "S" | "Z";
export type Colour = "yellow" | "aqua" | "orange" | "blue" | "lime" | "red" | "violet";
export type RotationState = 0 | 1 | 2 | 3; // Each tetromino has 4 rotation states (each 90 degrees)
export type Level = 1 | 2 | 3 | 4 | 5;
export type Axis = "x" | "y" | "xy"; // xy represents both axis, used for rotation (specify check for both axis)

export type SingleBlock = Readonly<{
  id: number,
  x: number,
  y: number,
  colour: Colour
}>;

export type Tetromino = Readonly<{
  type: TetroType,
  blocks: ReadonlyArray<SingleBlock>,
  rotationState: RotationState,
  pivot: SingleBlock,
}>

export type State = Readonly<{
  gameEnd: boolean,
  tetromino: Tetromino,
  next: Tetromino,
  staticBlocks: ReadonlyArray<SingleBlock>,
  exitBlocks: ReadonlyArray<SingleBlock>,
  blockCount: number,
  level: Level,
  score: number,
  highscore: number,
  rng: RNG
}>;

