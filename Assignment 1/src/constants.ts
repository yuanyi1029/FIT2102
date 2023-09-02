import { Vec } from "./util";

/** Constants */
export const Viewport = {
  CANVAS_WIDTH: 200,
  CANVAS_HEIGHT: 400,
  PREVIEW_WIDTH: 160,
  PREVIEW_HEIGHT: 80,
} as const;
  
export const Constants = {
  TICK_RATE_MS: 33.34, // For 30 frames per second. 1000 ms / 30 frames = 33.33
  LEVEL_0_TICK: 24, // Ticks before moving a tetromino down
  GRID_WIDTH: 10,
  GRID_HEIGHT: 20,
} as const;

export const Block = {
  WIDTH: Viewport.CANVAS_WIDTH / Constants.GRID_WIDTH,
  HEIGHT: Viewport.CANVAS_HEIGHT / Constants.GRID_HEIGHT,
};

/** Offset Constants used in rotation */
// Offset data for "O" tetromino
export const OffsetO = [[new Vec(), new Vec(0, -1), new Vec(-1, -1), new Vec(-1, 0)]]
// Offset data for "I" tetromino
export const OffsetI = [
    [new Vec(), new Vec(-1, 0), new Vec(-1, 1), new Vec(0, 1)],
    [new Vec(-1, 0), new Vec(), new Vec(1, -1), new Vec(0, 1)],
    [new Vec(2, 0), new Vec(), new Vec(-2, 1), new Vec(0, 1)],
    [new Vec(-1, 0), new Vec(0, 1), new Vec(1, 0), new Vec(0, -1)],
    [new Vec(2, 0), new Vec(0, -2), new Vec(-2, 0), new Vec(0, 2)],
]
// Offset data for all other tetrominoes
export const OffsetAll = [
  [new Vec(), new Vec(), new Vec(), new Vec()],
  [new Vec(), new Vec(1, 0), new Vec(), new Vec(-1, 0)],
  [new Vec(), new Vec(1, -1), new Vec(), new Vec(-1, -1)],
  [new Vec(), new Vec(0, 2), new Vec(), new Vec(0, 2)],
  [new Vec(), new Vec(1, 2), new Vec(), new Vec(-1, 2)],
]