---
title: Week 11 Exercises
author: FIT2102 Programming Paradigms
margin: 1inch
---

## Wordle

Welcome, intrepid developer, to the realm of code where the challenge at hand is to craft your very own digital incarnation of the beloved Wordle game. In this linguistic adventure, your mission is to replicate the essence of Wordle—a game of wit, deduction, and vocabulary mastery.

In your coding journey, envision a canvas where words become enigmatic puzzles, waiting to be deciphered. The player, equipped with lexical prowess, shall attempt to unravel a concealed word within a limited number of attempts. The thrill of the game lies in the feedback provided after each guess—a ballet of colors indicating correct letters in their right positions, misplaced letters, or complete misfires.

Embark on the multi-faceted odyssey of creating your own Wordle clone. This game requires combining all of the knowledge required throughout the semester, using observables to handle game logic, and communication. Using parsing to decode the messages between the front-end and the server side haskell, and using your haskell skills to develop the back-end logic for Wordle. Luckily, we have given you the first two, and you will only be required to develop the haskell code, future students may not be so lucky. But consider this as a fully fleshed out exercise, using only functional techniques to create a game and server-side logic. 

## Usage

Build the bundle using

```bash
$ stack build
```

This might take a lot longer this week, as we will be introducing new dependencies to handle communication with the browser!

**Web**

The web version needs to be run in two terminals, similar to the assignment.

First, start the server from the main directory using

```bash
$ stack exec main
```

then open a new terminal and navigate to `/javascript` and run

```bash
$ npm install
$ npm run dev
```

This should now start the front-end.

### Suggested order

All of the exercises to be implemented are in `Wordle.hs`.

1. Implement up to `makeFeedback'` which is the core Wordle logic
2. Implement the hard mode exercises
3. Test your implementation using the web version. See [Usage](#usage) for how to do this.

## Part 1: Wordle Feedback
In this initial phase, lay the foundation for your Wordle clone. Create information about misplaced letters or incorrect choices. Craft the logic with precision, ensuring that every code snippet contributes to the seamless interaction between player and game. As you delve into the world of arrays and conditional statements, let the this phase be a testament to the elegance of foundational coding.

## Part 2: Hard Mode Wordle
Elevate your Wordle clone to the zenith of challenge with the introduction of the Hard Mode. In this advanced phase, the users ability to play the game will be restricted, and more challenges await.

May your text editor be a nimble spacecraft, your compiler a reliable navigation system, and your tests as constant as the North Star. Your success is the supernova at the end of this odyssey, where flawless code and passing tests shall be your cosmic legacy.

May the gravitational forces of clean code be ever in your favor as you venture into the unknown territories of this Galactic Coding Odyssey. Safe travels.


## Part 3 (Optional): Optional Hard Word Mode:

Add a toggle to the HTML page, which will allow the user to toggle between hard mode and normal Wordle. This will require you passing the information over the POST request to haskell and handling it appropriately! The UI should be handled with Observables too, relating back to your work from earlier in the semester. 
