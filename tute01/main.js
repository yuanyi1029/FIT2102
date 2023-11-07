/**
 * IMPORTANT: The README.md file contains important information on getting started.
 * Please read before attempting these exercises.
 *
 * You must make all the tests pass, indicated by a green ✓, and
 * follow all the instructions given in the code file for each exercise.
 *
 * Note that passing tests is just an indication the implementation is not incorrect.
 * Marks are only awarded for correct understanding of the question and demonstration of concepts.
 *
 * Passing all the tests is not sufficient to receive (full) marks.
 * Make sure you understand the question and your solution.
 *
 * Ask a tutor if in doubt!
 */

// Stub value to indicate an implementation
const IMPLEMENT_THIS = undefined;

/*****************************************************************
 * Exercise 1:
 *
 * The const keyword is used for creating an un-reassignable variable.
 * see: https://tgdwyer.github.io/javascript1#declaring-variables
 *
 * Create an un-reassignable variable called firstConst and initialise its value to 1.
 *
 * Then create another un-reassignable variable called secondConst
 *  and initialise its value to firstConst + 1.
 */
const firstConst = 1;
const secondConst = firstConst + 1;

/*****************************************************************
 * Exercise 2:
 *
 * For this exercise, each function should initialise a local variable with the value 2
 * and return double its value.
 * see: https://tgdwyer.github.io/javascript1/#functions
 *
 * Create the following functions, each using a different way to declare a function:
 *  - aFunction: using the "function" keyword
 *  - anonymousFunction: using an anonymous function declared with the "function" keyword
 *  - arrowFunction: using an arrow function
 */
function aFunction() {
  const localVar = 2;
  return localVar * 2;
}

const anonymousFunction = function () {
  const localVar = 2;
  return localVar * 2;
}

const arrowFunction = () => {
  const localVar = 2;
  return localVar * 2;
}

/*****************************************************************
 * Exercise 3:
 *
 * Solve the Project Euler Problem 1 by implementing the following functions.
 *
 * Project Euler Problem 1 is the following:
 *
 *  "If we list all the natural numbers below 10 that are multiples of 3 or 5,
 *   we get 3, 5, 6 and 9. The sum of these multiples is 23.
 *   Find the sum of all the multiples of 3 or 5 below 1000."
 *
 * You will need to use recursion, immutable variables, and higher order functions.
 */

/**
 * @param x Number to check
 * @returns True if x is divisible by three or five, false otherwise
 */
const isDivisibleByThreeOrFive = (x) => {
  return (x % 3 === 0 || x % 5 === 0)
};

/**
 * Sum up to a specified number, ignoring values.
 *
 * /Hint/: This should be recursive
 *
 * @param f Function that returns true if we should keep the value, false otherwise
 * @param n Number to sum to (inclusive)
 * @returns the sum
 */
const selectiveSummer = (f) => (n) => {
  const summer_aux = (n) => {
    return n === 0 ? 0 : (f(n) ? n + summer_aux(n - 1) : summer_aux(n - 1))
  };

  return summer_aux(n);
};

/**
 * Solves the project euler problem 1 for some number
 *
 * /Hint/: Remember to exclude the target number from the sum
 *
 * @param n Target number
 * @returns The sum of numbers up to but not including n that are divisible by three or five
 */
const filteredSum = (n) => selectiveSummer(isDivisibleByThreeOrFive)(n - 1);

/**
 * @returns Answer to project euler problem 1
 */
const projectEulerProblem1 = () => filteredSum(1000);

/*****************************************************************
 * Exercise 4:
 *
 * Practise using array methods.
 * see: https://tgdwyer.github.io/javascript1/#arrays
 */

/**
 * Print all the numbers in an array
 *
 * /Hint/: console.log is used to print
 *
 * @param arr Array to print
 */
const printArray = (arr) => {
  console.log(arr);
};

/**
 * Create a new array with each item incremented by one (1)
 *
 * @param arr Array to increment
 * @returns New array with incremented items
 */
const addOne = (arr) => {
  return arr.map(num => num + 1);
}; 

/**
 * Create a new array with ones removed
 *
 * @param arr Input array
 * @returns Array without ones
 */
const removeOnes = (arr) => {
  return arr.filter(num => num !== 1);
};

/**
 * (Optional) Add two numbers
 *
 * @param x First number
 * @param y Second number
 * @returns The sum of x and y
 */
const add = (x, y) => {
  return x + y;
};

/**
 * Calculate the sum of the items in an array
 *
 * @param arr Input array
 * @returns Sum of items in arr
 */
const sumArray = (arr) => {
  return arr.reduce((accumulator, num) => accumulator + num, 0)
};

/*****************************************************************
 * Exercise 5:
 *
 * Solve the Project Euler Problem 1 by implementing the following functions.
 *
 * This time we use array methods instead of recursion.
 */

/**
 * Create a range of values
 *
 * @param n Target value
 * @returns Array of integers in the range [0, n)
 */
const range = (n) => {
  return Array(n).fill().map((_, i) => i);
};

/**
 * @returns Answer to project euler problem 1
 */
const projectEulerProblem1Again = () => range(1000).filter(each => each % 3 === 0 || each % 5 === 0).reduce((accumulator, each) => accumulator + each, 0);

/*****************************************************************
 * Exercise 6:
 * This exercise starts to explore functional programming concepts.
 * We will go through this in detail in the Workshop.
 *
 * The Cons list is a simple immutable data structure composing
 * only of functions, using closures to capture data.
 * see also: https://tgdwyer.github.io/functionaljavascript/#computation-with-pure-functions
 *
 * This is essentially equivalent to linked lists.
 *
 * Implement the following functions to define a Cons list.
 */

/**
 * Cons "constructs" a list node, if no second argument is specified it is the last node in the list
 *
 * @param head Head of cons list, the value to be stored
 * @param rest Tail of cons list, reference to the rest of the cons list
 * @returns Cons list, function in closure
 */
function cons(head, rest = null) {
  return (selector) => selector(head, rest);
}

/**
 * Head selector
 *
 * @param list Non-empty cons list, remember this is a function!
 * @returns First element in cons list
 */
function head(list) {
  if (!list) throw new TypeError("list is null");

  return list((head, rest) => head);
}

/**
 * Rest selector
 *
 * @param list Non-empty cons list, remember this is a function!
 * @returns Rest of the cons list
 */
function rest(list) {
  if (!list) throw new TypeError("list is null");

  return list((head, rest) => rest);
}

/*****************************************************************
 * Exercise 7
 * This exercise starts to explore functional programming concepts.
 * We will go through this in detail in the Workshop.
 * see https://tgdwyer.github.io/functionaljavascript#computation-with-pure-functions
 *
 * Higher order functions are applicable on any data type we can think of.
 * This, of course, also applies to the Cons list.
 *
 * Implement the following higher order functions for Cons lists.
 */

/**
 * Use this as an example for other functions!
 *
 * @param f Function to use for each element
 * @param list Cons list
 */
function forEach(f, list) {
  if (list) {
    f(head(list));
    forEach(f, rest(list));
  }
}

/**
 * Map for cons list
 *
 * @param f Function to apply
 * @param list Cons list to map
 * @returns New cons list with f applied to elements
 */
function map(f, list) {
  if (!list) return null;

  return cons(f(head(list)), map(f, rest(list)));
}

/**
 * Reduce for cons list
 *
 * @param {(acc, val) => any} f Reducing function, this combines the accumulator with the current value. Note that the accumulator value is the first parameter, and the current value is the second parameter.
 * @param acc Accumulated value, initial value
 * @param list Cons list to reduce
 * @returns The accumulated value after applying f to each element in list
 */
function reduce(f, acc, list) {
  if (!list) return acc;

  return reduce(f, f(acc, head(list)), rest(list));
}

/**
 * Filter for cons list
 *
 * @param f Function to accept or reject values
 * @param list Cons list to filter
 * @returns New cons list with only accepted values
 */
function filter(f, list) {
  if (!list) return null;

  // Skip value
  if (!f(head(list))) return filter(f, rest(list));

  return cons(head(list), filter(f, rest(list)));
}