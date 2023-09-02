/** Utility functions */
export const not = <T>(f:(x:T) => boolean) => (x:T) => !f(x);
export const elem = <T>(eq: (_:T) => (_:T) => boolean) => (a:ReadonlyArray<T>) => (e:T) => a.findIndex(eq(e)) >= 0;
export const except = <T>(eq: (_:T) => (_:T) => boolean) => (a:ReadonlyArray<T>) => (b:ReadonlyArray<T>) => a.filter(not(elem(eq)(b)));
export const isNotNullOrUndefined = <T extends object>(input: null | undefined | T): input is T => input != null;

/**
 * Helper Vector class used to represent and calculate offsets in rotation.
 */
export class Vec {
  constructor(public readonly x: number = 0, public readonly y: number = 0) {}
  /**
   * Adds the x and y values between self and another Vector class
   * @param b Another Vector object
   * @returns new Vector object after addition
   */
  add = (b: Vec) => new Vec(this.x + b.x, this.y + b.y)
  /**
   * Subtracts the x and y values between self and another Vector class
   * @param b Another Vector object
   * @returns new Vector object after subtraction
   */
  sub = (b: Vec) => new Vec(this.x - b.x, this.y - b.y)
  static Zero = new Vec()
}
  
/**
 * Helper RNG class used to generate random numbers from a seed
 */
export class RNG {
  constructor(public readonly seed: number) {}
  // LCG using GCC's constants
  private static m = 0x80000000; // 2**31
  private static a = 1103515245;
  private static c = 12345;

  /**
   * Call `hash` repeatedly to generate the sequence of hashes.
   * @returns a hash of the seed
   */
  public hash = () => (RNG.a * this.seed + RNG.c) % RNG.m;
  /**
   * Takes hash value and scales it to the range [0, 7]
   * @returns a scaled value of the hashed value
   */
  public scale = () => (this.hash() % 7);
  /**
   * Creates a new RNG object for a different random number
   * @returns a new RNG object with a different seed 
   */
  public next = () => new RNG(this.hash());
}