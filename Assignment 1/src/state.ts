import { SingleBlock, Tetromino, State, Level, Axis } from "./types";
import { Vec, RNG, except } from "./util";
import { Constants, OffsetO, OffsetI, OffsetAll } from "./constants";

/**
 * Initial Seed and RNG class used to randomly create tetrominoes
 * Initial Seed is created from the user PC's time in milliseconds.
 */
const initialSeed = new Date().getMilliseconds()
const initialRNG = new RNG(initialSeed)

/**
 * Creates a random Tetromino
 * @param blockCount Current block count to assign to block's id 
 * @param index index to get a tetromino from tetromino array
 * @returns created Tetromino
 */
const createTetromino = (blockCount: number, index: number): Tetromino => {
  const bricks = [
    {
      type: "O",
      blocks: [
        {id: blockCount    , x: 4, y: 18, colour: "yellow"} as SingleBlock, 
        {id: blockCount + 1, x: 5, y: 18, colour: "yellow"} as SingleBlock, 
        {id: blockCount + 2, x: 4, y: 19, colour: "yellow"} as SingleBlock, 
        {id: blockCount + 3, x: 5, y: 19, colour: "yellow"} as SingleBlock
      ],
      rotationState: 0,
      pivot: {id: blockCount, x: 4, y: 18, colour: "yellow"} as SingleBlock
    } as Tetromino,
    {
      type: "I",
      blocks: [
        {id: blockCount    , x: 4, y: 19, colour: "aqua"} as SingleBlock, 
        {id: blockCount + 1, x: 3, y: 19, colour: "aqua"} as SingleBlock, 
        {id: blockCount + 2, x: 5, y: 19, colour: "aqua"} as SingleBlock, 
        {id: blockCount + 3, x: 6, y: 19, colour: "aqua"} as SingleBlock
      ],
      rotationState: 0,
      pivot: {id: blockCount, x: 4, y: 19, colour: "aqua"} as SingleBlock
    } as Tetromino,
    {
      type: "J",
      blocks: [
        {id: blockCount    , x: 5, y: 18, colour: "blue"} as SingleBlock, 
        {id: blockCount + 1, x: 4, y: 19, colour: "blue"} as SingleBlock, 
        {id: blockCount + 2, x: 4, y: 18, colour: "blue"} as SingleBlock, 
        {id: blockCount + 3, x: 6, y: 18, colour: "blue"} as SingleBlock
      ],
      rotationState: 0,
      pivot: {id: blockCount, x: 5, y: 18, colour: "blue"} as SingleBlock
    } as Tetromino,
    {
      type: "L",
      blocks: [
        {id: blockCount    , x: 5, y: 18, colour: "orange"} as SingleBlock, 
        {id: blockCount + 1, x: 4, y: 18, colour: "orange"} as SingleBlock, 
        {id: blockCount + 2, x: 6, y: 18, colour: "orange"} as SingleBlock, 
        {id: blockCount + 3, x: 6, y: 19, colour: "orange"} as SingleBlock
      ],
      rotationState: 0,
      pivot: {id:  blockCount, x: 5, y: 18, colour: "orange"} as SingleBlock
    } as Tetromino,
    {
      type: "S",
      blocks: [
        {id: blockCount    , x: 5, y: 18, colour: "lime"} as SingleBlock, 
        {id: blockCount + 1, x: 4, y: 18, colour: "lime"} as SingleBlock, 
        {id: blockCount + 2, x: 5, y: 19, colour: "lime"} as SingleBlock, 
        {id: blockCount + 3, x: 6, y: 19, colour: "lime"} as SingleBlock
      ],
      rotationState: 0,
      pivot: {id: blockCount, x: 5, y: 18, colour: "lime"} as SingleBlock
    } as Tetromino,
    {
    type: "Z",
      blocks: [
        {id: blockCount    , x: 5, y: 18, colour: "red"} as SingleBlock, 
        {id: blockCount + 1, x: 4, y: 19, colour: "red"} as SingleBlock, 
        {id: blockCount + 2, x: 5, y: 19, colour: "red"} as SingleBlock, 
        {id: blockCount + 3, x: 6, y: 18, colour: "red"} as SingleBlock
      ],
      rotationState: 0,
      pivot: {id: blockCount, x: 5, y: 18, colour: "red"} as SingleBlock
    } as Tetromino,
    {
      type: "T",
      blocks: [
        {id: blockCount    , x: 5, y: 18, colour: "violet"} as SingleBlock, 
        {id: blockCount + 1, x: 4, y: 18, colour: "violet"} as SingleBlock, 
        {id: blockCount + 2, x: 5, y: 19, colour: "violet"} as SingleBlock, 
        {id: blockCount + 3, x: 6, y: 18, colour: "violet"} as SingleBlock
      ],
      rotationState: 0,
      pivot: {id: blockCount, x: 5, y: 18, colour: "violet"} as SingleBlock
    } as Tetromino,
  ]
  return bricks[index]
}

/**
 * Initial State
 */
export const initialState: State = {
  gameEnd: false,
  tetromino: createTetromino(0, initialRNG.scale()),
  next: createTetromino(4, initialRNG.next().scale()),
  staticBlocks: [],
  exitBlocks: [],
  blockCount: 4,
  level: 1,
  score: 0,
  highscore: 0,
  rng: initialRNG.next().next()
} as const;

/**
 * Action Interface, each class that extends from Action must have
 * an apply function
 */
export interface Action {
  apply(s: State): State;
}

/**
 * MoveX class, for a tetromino's movement on x axis, instantiated when "A" or "D"
 * key is pressed
 * @param amount amount for how much a tetromino moves on the x axis
 */
export class MoveX implements Action {constructor(public readonly amount: number) {}
  apply(s: State): State {
    // Check is a block will collide (with wall/other blocks), update tetromino accordingly
    const isValidMove = !currentBlockWillCollide(s.tetromino, s.staticBlocks, "x", this.amount);
    const updatedState = {...s, 
      tetromino: isValidMove ? moveBlock(s.tetromino, "x", this.amount) : s.tetromino
    } as State
    return updatedState
  }
}

/**
 * MoveY class, for a tetromino's movement on y axis, instantiated when "S" key is 
 * pressed
 * @param amount amount for how much a tetromino falls on the y axis
 */
export class MoveY implements Action {constructor(public readonly amount: number) {}
  apply(s: State): State {
    return handleDown(s)
  }
}

/**
 * Tick class, for a tetromino's movement during a single tick (tetromino falls) by 1 block
 * @param elapsed amount of ticks already happened
 */
export class Tick implements Action {constructor(public readonly elapsed: number) {}
  apply(s: State): State {
    // Only call handleDown(s) per few ticks depending on the level
    // Higher level - call handleDown(s) per less ticks, Lower level - call handleDown(s) per more ticks
    const updatedState = this.elapsed % translateLevel(s.level) === 0 ? handleDown(s) : s
    return updatedState
  }
}

/**
 * Rotate class, for a tetromino's rotation, instantiated when "W" key is pressed
 */
export class Rotate implements Action {constructor() {}
  apply(s: State): State {
    // Rotate the tetromino, then get possible tetrominoes based on an offset 
    const finalPositionBlocks = rotateBlocks(s.tetromino, s.tetromino.pivot)
    const possbleBlocks = 
    (s.tetromino.type === "O" ? addOffset(finalPositionBlocks, OffsetO) : 
    s.tetromino.type === "I" ? addOffset(finalPositionBlocks, OffsetI) : 
    addOffset(finalPositionBlocks, OffsetAll))
    .filter(eachSet => !currentBlockWillCollide(eachSet, s.staticBlocks, "xy"))
    
    // Update the tetromino if there are available possible tetrominoes, else remain no rotation,
    // The updated pivot is coded to be always the first block in the blocks array
    const updatedTetromino = {...s.tetromino, 
      blocks: possbleBlocks.length > 0 ? possbleBlocks[0].blocks : s.tetromino.blocks,
      rotationState: possbleBlocks.length > 0 ? (s.tetromino.rotationState + 1) % 4 : s.tetromino.rotationState,
      pivot: possbleBlocks.length > 0 ? possbleBlocks[0].blocks[0] : s.tetromino.pivot
    } as Tetromino 
    
    return {...s,
      tetromino: updatedTetromino
    } as State
  }
}

/**
 * Restart class, for a game's restart, instantiated when "T" key is pressed
 */
export class Restart implements Action {constructor() {}
  apply(s: State): State {
    // Only allow restart upon gameEnd
    const updatedState = s.gameEnd ? {...initialState,
      highscore: s.highscore
    } as State : s
    return updatedState
  }
}

/**
 * Translates level into an interval to move the block down
 * @param level current game level
 * @returns a number representing how many ticks it should wait before a block going down
 */
const translateLevel = (level: Level): number => {
  // level 1: 20 ticks before tetromino moves down
  // level 2: 16 ticks before tetromino moves down
  // level 3: 12 ticks before tetromino moves down
  // level 4: 8 ticks before tetromino moves down
  // level 5: 4 ticks before tetromino moves down
  // Formula = 24 - (level * 4)
  return Constants.LEVEL_0_TICK - (level * 4)
} 

/**
 * Checks if a tetromino will collide with other blocks, walls or the floor depending on the axis
 * @param tetromino current moving tetromino
 * @param staticBlocks blocks that are already on the grid, not moving
 * @param axis The current moving axis
 * @param amount amount that a tetromino block moves on the x or y axis
 * @returns a boolean if a tetromino would collide
 */
const currentBlockWillCollide = (tetromino: Tetromino, staticBlocks: ReadonlyArray<SingleBlock>, axis: Axis, amount: number = 0) => {    
  // Map through tetromino blocks to check for exceeding blocks with floor or top (y + amount < 0 OR y + amount > 19)
  const exceedFloor = axis === "y" || axis === "xy" ? tetromino.blocks.reduce((acc, eachBlock) => acc || (eachBlock.y + amount < 0 || eachBlock.y + amount > Constants.GRID_HEIGHT - 1), false) : false
  // Map through tetromino blocks to check for exceeding blocks with walls (x + amount < 0 OR x + amount > 9)
  const exceedWall = axis === "x" || axis === "xy" ? tetromino.blocks.reduce((acc, eachBlock) => acc || (eachBlock.x + amount < 0 || eachBlock.x + amount > Constants.GRID_WIDTH - 1), false) : false

  // Map through tetromino blocks and all blocks in static, check if amount is the same with any existing blocks
  const collideBlock = tetromino.blocks.reduce(
    (acc, eachBlock) => acc || (staticBlocks.reduce(
      (acc, eachSBlock) => acc || (axis === "y" ? 
        eachBlock.x === eachSBlock.x && eachBlock.y + amount === eachSBlock.y : 
        eachBlock.x + amount === eachSBlock.x && eachBlock.y === eachSBlock.y)
    , false)) 
  , false)

  return (exceedFloor || exceedWall || collideBlock)
}

/**
 * Moves blocks of a tetromino by the x axis or y axis, returns a moved Tetromino
 * @param tetromino current moving tetromino
 * @param axis axis for tetromino to move on
 * @param amount amount that a tetromino block moves on the x or y axis
 * @returns a moved Tetromino
 */
const moveBlock = (tetromino: Tetromino, axis: Axis, amount: number): Tetromino => {
  // Map through tetromino blocks and increase / decrease x or y value by amount, same for pivot
  const updatedTetrominoBlocks = tetromino.blocks.map(eachBlock => ({...eachBlock,
    x: axis === "x" ? Math.min(Math.max(eachBlock.x + amount, 0), Constants.GRID_WIDTH - 1) : eachBlock.x,
    y: axis === "y" ? Math.max(eachBlock.y + amount, 0) : eachBlock.y
  } as SingleBlock))

  const updatedPivot = {...tetromino.pivot, 
    x: axis === "x" ? Math.min(Math.max(tetromino.pivot.x + amount, 0), Constants.GRID_WIDTH - 1) : tetromino.pivot.x,
    y: axis === "y" ? Math.max(tetromino.pivot.y + amount, 0) : tetromino.pivot.y
  } as SingleBlock

  return {...tetromino,
    blocks: updatedTetrominoBlocks,
    pivot: updatedPivot 
  } as Tetromino
}

/**
 * Determines level based on the current score
 * @param score current game score
 * @returns the Level of the game based on the score
 */
const determineLevel = (score: number): Level => {
  // 0: level 1
  // 300: level 2
  // 600: level 3
  // 1000: level 4
  // 1500: level 5
  const level = score > 200 ?  score > 500 ? score > 900 ? score > 1400 ? 5 : 4 : 3 : 2 : 1 
  return level
}

/**
 * Handles the Down Movement of a tetromino, updates the State
 * HandleDown checks for valid moves or game end
 * Order: handleDown() -> handleClear() -> updateRows(), updateRows() is optional
 * @param s current state
 * @returns updated State
 */
const handleDown = (s: State): State => {
  // Check for valid move when going down or gameEnd, game ends when a spawned tetromino is spawned "On top" of an existing tetromino 
  const isValidMove = !(currentBlockWillCollide(s.tetromino, s.staticBlocks, "y", -1))
  const gameEnd = s.tetromino.blocks.reduce((acc, eachBlock) => acc || s.staticBlocks.reduce((acc, eachSBlock) => acc || eachBlock.x === eachSBlock.x && eachBlock.y === eachSBlock.y, false), false)

  // updateState based on gameEnd, if game end, update highscore, else update state based on validity of tetromino movement
  const updatedState = gameEnd ? {...s,
    gameEnd: gameEnd,
    highscore: s.score > s.highscore ? s.score : s.highscore 
  } as State : {...s,
    tetromino: isValidMove ? moveBlock(s.tetromino, "y", -1) : s.next,
    next: isValidMove ? s.next : createTetromino(s.blockCount + 4, s.rng.scale()),
    staticBlocks: isValidMove ? s.staticBlocks : s.staticBlocks.concat(s.tetromino.blocks), 
    blockCount: isValidMove ? s.blockCount : s.blockCount + 4,
    rng: s.rng.next()
  } as State

  return handleClear(updatedState)
}
  
/**
 * Handles the Clearane of rows for staticBlocks, updates the State
 * HandleClear checks if there are any rows that are full
 * @param s current state
 * @returns updated State
 */
const handleClear = (s: State): State => { 
  const cut = except((a: SingleBlock) => (b: SingleBlock) => a.id === b.id)
  // Map through all static blocks, find unique y values (occupiedRows), filter and keep row numbers with 10 blocks (rowsToClear)  
  const occupiedRows = s.staticBlocks.map(eachBlock => eachBlock.y).filter((value, index, output) => output.indexOf(value) === index)
  const rowsToClear = occupiedRows.filter(eachRowNumber => {
    return s.staticBlocks.filter(eachBlock => eachBlock.y === eachRowNumber).length === Constants.GRID_WIDTH
  })

  // Map through staticBlocks, filter and keep blocks that have matching y values with rowsToClear, updateState accordingly
  const blocksToClear = s.staticBlocks.filter(eachBlock => rowsToClear.includes(eachBlock.y))
  const clearRequired = rowsToClear.length > 0
  const newScore = s.score + rowsToClear.length * 100
  const updatedState =  {...s,
    staticBlocks: cut(s.staticBlocks)(blocksToClear),
    exitBlocks: s.exitBlocks.concat(blocksToClear),
    level: determineLevel(newScore),
    score: newScore, 
  }

  // call updateRows if a clear happened (to shift blocks downwards)
  if (clearRequired) {
    return updateRows(updatedState, rowsToClear)
  }
  return updatedState 
}

/**
 * Handles the updating of remaining uncleared blocks for staticBlocks, updates the State
 * updateRows updates all blocks that were not cleared, either shifting
 * them down or staying in place
 * @param s current state
 * @param clearedRows array of row numbers that were cleared
 * @returns updated State
 */
const updateRows = (s: State, clearedRows: ReadonlyArray<number>): State => {
  // Reduce through clearedRows to determine lowestClear Row, find all blocks that are above the lowestClearRow 
  const lowestClearRow = clearedRows.reduce((acc, rowNum) => rowNum < acc ? rowNum : acc, clearedRows[0])
  const blocksToUpdate = s.staticBlocks.filter(eachBlock => eachBlock.y > lowestClearRow)
  
  // Update all blocks that need updating, determine how many rows cleared below it, reduce y by that amount
  const updatedBlocks = blocksToUpdate.map(eachBlock => ({...eachBlock,
    y: eachBlock.y - (clearedRows.reduce((acc, rowNum) => rowNum < eachBlock.y ? acc + 1: acc, 0))
  } as SingleBlock))

  const updatedState = {...s,
    staticBlocks: s.staticBlocks.filter(eachBlock => !blocksToUpdate.includes(eachBlock)).concat(updatedBlocks)
  } as State

  return updatedState
}
  
/**
 * Rotates the blocks of a tetromino by its pivot
 * @param s current state
 * @returns rotated Tetromino
 */
const rotateBlocks = (tetromino: Tetromino, pivot: SingleBlock): Tetromino => {
  // Map through blocks in a tetromino, find relative positions to pivot (block coordinate - pivot coordinate)
  // Calculate final positions of each block using a clockwise rotation matrix and adding pivot coordinates
  // [0, 1]    [x]
  // [-1, 0] x [y]
  // Update each block and return a new Tetromino
  const updatedBlocks = tetromino.blocks.map(eachBlock => {
    const xRelativeToPivot = eachBlock.x - pivot.x
    const yRelativeToPivot = eachBlock.y - pivot.y
    const xFinalPosition = (xRelativeToPivot * 0 + yRelativeToPivot * 1) + pivot.x
    const yFinalPosition = (xRelativeToPivot * -1 + yRelativeToPivot * 0) + pivot.y
    return {...eachBlock,
      x: xFinalPosition,
      y: yFinalPosition,
    } as SingleBlock
  })
  return {...tetromino,
    blocks: updatedBlocks,
  } as Tetromino
}

/**
 * Adds different offsets to tetromino blocks 
 * @param tetromino current moving tetromino
 * @param offset an array of Vector arrays
 * @returns an array of possible Tetromino objects
 */
const addOffset = (tetromino: Tetromino, offset: ReadonlyArray<ReadonlyArray<Vec>>): ReadonlyArray<Tetromino> => {
  // Map through offsets, determine initial and final position offset, subract them: (initial offset) - (final offset)
  // Use the calculated offset and add it to either x or y properties for each block in a tetromino
  return offset.map(eachOffset => {
    const initial = eachOffset[tetromino.rotationState] 
    const final = eachOffset[(tetromino.rotationState + 1) % 4] 
    const updatedBlocks =  tetromino.blocks.map(eachBlock => ({...eachBlock, x: eachBlock.x + (initial.sub(final).x), y: eachBlock.y + (initial.sub(final).y)} as SingleBlock))
    return {...tetromino, 
      blocks: updatedBlocks
    } as Tetromino
  })
}