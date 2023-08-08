"use strict";
/**
 * IMPORTANT: The README file contains important information on
 * getting started.
 * Please read before attempting these exercises.
 *
 * You must make all the tests pass, indicated by a green ✓, and
 * follow all the instructions given in the code file for each exercise.
 *
 * The code must compile with no Typescript warnings or errors.
 *
 * Marks are only awarded for correct understanding of the question
 * and demonstration of concepts.
 *
 * Completing the tasks with correctly compiling code does not guarantee
 * full marks.
 *
 * Make sure you understand the question and your solution.
 *
 * Ask a tutor if in doubt!
 *
 * **There are hints throughout these exercises encoded in base64.**
 * You can use online tools such as https://www.base64decode.org/
 * to decode them.
 *
 * **Reminders**
 *
 * You must **not** use for-loops, while-loops, or similar imperative
 * techniques in these exercises unless explicitly approved, required,
 * or provided.
 *
 * All code must also be pure and values immutable.
 *
 * This means declaring variables with `const`, using `Readonly` and
 * `... as const` to ensure immutable types, and avoiding using mutating
 * methods such as `Array.prototype.push()`.
 */
// Stub value to indicate an implementation
const IMPLEMENT_THIS = undefined;
const outputText = (targetDiv, text) => (document.getElementById(targetDiv).innerHTML =
    "<pre>Output:\n\n" + (text || "Not Implemented") + "</pre>");
/*****************************************************************
 * Exercise 1
 *
 * Create an object called anObject with the property "x" set to 5.
 *
 * Create another object object called anotherObject that
 *  has the properties of anObject, and
 *  has the property "y" set to 10,
 * using the spread operator.
 *
 * Make sure to NOT mutate anything, including objects.
 *
 * see: https://tgdwyer.github.io/typescript1/#using-the-compiler-to-ensure-immutability
 */
const anObject = {
    x: 5
};
const anotherObject = Object.assign(Object.assign({}, anObject), { y: 10 });
const todo = {
    timestamp: 65535,
    from: "https://jsonplaceholder.typicode.com/todos",
    // Base64 encoding of something ... I wonder what ((⇀‸↼))
    verySecretVerySecureCodeThatDefinitelyDoesNotContainAnythingUseful: "bmV2ZXIgZ29ubmEgZ2l2ZSB5b3UgdXAKbmV2ZXIgZ29ubmEgbGV0IHlvdSBkb3duCm5ldmVyIGdvbm5hIHJ1biBhcm91bmQKYW5kIGRlc2VydCB5b3UKCm5ldmVyIGdvbm5hIG1ha2UgeW91IGNyeQpuZXZlciBnb25uYSBzYXkgZ29vZGJ5ZQpuZXZlciBnb25uYSB0ZWxsIGEgbGllCmFuZCBodXJ0IHlvdQ==",
    content: [
        {
            userId: 1,
            id: 1,
            title: "delectus aut autem",
            completed: false,
        },
        {
            userId: 1,
            id: 2,
            title: "quis ut nam facilis et officia qui",
            completed: false,
        },
        {
            userId: 1,
            id: 3,
            title: "fugiat veniam minus",
            completed: false,
        },
    ],
};
/*****************************************************************
 * Exercise 2b
 *
 * Implement the following functions to pretty print this mock
 *  response of todo data.
 *
 * Note that the field
 * `verySecretVerySecureCodeThatDefinitelyDoesNotContainAnythingUseful`
 * is not included in the pretty printing.
 *
 * ! Must use higher order functions !
 * ! Do NOT hardcode the number of items !
 *
 * Suggested format:
 *
 * timestamp: 65535
 * from: https://jsonplaceholder.typicode.com/todos
 * content:
 * userId: 1 | id: 1 | title: delectus aut autem | completed: false
 * userId: 1 | id: 2 | title: quis ut nam facilis et officia qui | completed: false
 * userId: 1 | id: 3 | title: fugiat veniam minus | completed: false
 *
 * You may choose a different format if you wish as long as it is
 * reasonably readable.
 *
 * see: https://tgdwyer.github.io/javascript1/#array-cheatsheet
 */
/**
 *
 * /Hint/: Useful functions T2JqZWN0LmVudHJpZXMKQXJyYXkucHJvdG90eXBlLmpvaW4=
 *
 * @param item Todo item
 * @returns Prettified todo item
 */
const prettifyTodoItem = (item) => {
    return Object.entries(item).map(each => each.join(": ")).join(" | ");
};
/**
 *
 * @param content Collection of todo items
 * @returns Prettified content
 */
const prettifyContent = (content) => {
    return content.reduce((accumulator, each) => accumulator + prettifyTodoItem(each) + "\n", "");
};
/**
 *
 * @param todo Todo object
 * @returns prettified representation of todo data
 *
 * /Hint/: String templates can make this look a lot nicer
 *  `some string ${variable_expression} some other string`
 *
 * /Hint 2/: Object destructuring can be used in the arguments
 *  e.g. ({ someKey }) => someKey
 */
const prettifyTodo = ({ timestamp, from, content }) => {
    // return `timestamp: ${timestamp} \nfrom: ${from} \ncontent: \n${prettifyContent(content)}`
    return Object.entries(todo)
        .filter(each => each[0] !== 'verySecretVerySecureCodeThatDefinitelyDoesNotContainAnythingUseful')
        .map(each => each[0] === 'content' ? `${each[0]}:\n${prettifyContent(each[1])}` : each.join(": "))
        .join("\n");
};
outputText("pretty_object_output", prettifyTodo(todo).toString());
/**
 * Creates a binary tree node
 *
 * /Hint/: Remember to declare the generic type!
 *
 * /Hint 2/: The ? is used to indicate an optional parameter
 *  e.g. (x: int, y?: int) => {}
 *
 * /Hint 3/: You may need to cast the object
 *
 * @param data Data to store at node
 * @param left Left child
 * @param right Right child
 * @returns Binary tree node
 */
const binaryTree = (data, left, right) => {
    return { data, left, right };
};
const binaryTreeExample = binaryTree(1, binaryTree(2, binaryTree(3)), binaryTree(4, binaryTree(5), binaryTree(6)));
/*****************************************************************
 * Exercise 3b
 *
 * Implement the following functions to pretty print a binary tree.
 *
 * What this means is that the value of each node is printed, and
 * the children are indented by a certain amount. Note that the tree
 * can be of any depth and may have missing children.
 *
 * /Hint/: Recursive data structures are very conducive to recursion!
 *
 * Suggested format:
 *
 * 1
 * |- 2
 * |- |- 3
 * |- 4
 * |- |- 5
 * |- |- 6
 *
 * You may choose a different format if you wish as long as it is
 * reasonably readable. Parent, children, and siblings must be
 * clearly distinguishable.
 *
 * /Note/: You can add parameters to functions if you wish, as long
 *  as it doesn't affect the current usage (i.e. tests). This can be
 *  done by adding an optional/default parameter.
 *  E.g.
 *    function prettifyBinaryTree<T extends Object>(node: BinaryTree<T>): string
 *    function prettifyBinaryTree<T extends Object>(node: BinaryTree<T>, anotherParameter = ""): string
 */
/**
 * Pretty print a binary tree
 *
 * @param node Root node of binary tree
 * @returns Prettified binary tree
 */
function prettifyBinaryTree(node, depth = 0, output = "") {
    const current = node.data.toString();
    const left = node.left;
    const right = node.right;
    if (current) {
        output += `${"|- ".repeat(depth)}${current}\n`;
        if (left)
            output = prettifyBinaryTree(left, depth + 1, output);
        if (right)
            output = prettifyBinaryTree(right, depth + 1, output);
    }
    return output;
}
const prettyBinaryTree = prettifyBinaryTree(binaryTreeExample);
outputText("pretty_btree_output", prettyBinaryTree);
const naryTree = (data, children = []) => {
    return { data, children };
};
const naryTreeExample = naryTree(1, [
    naryTree(2),
    naryTree(3, [naryTree(41), naryTree(42), naryTree(43)]),
    naryTree(5, [naryTree(6)]),
]);
/*****************************************************************
 * Exercise 4b
 *
 * Implement the following functions to pretty print an nary tree.
 *
 * /Hint/: What's the difference between 3b and 4b?
 *
 * Suggested format:
 *
 * 1
 * |- 2
 * |- 3
 * |- |- 41
 * |- |- 42
 * |- |- 43
 * |- 5
 * |- |- 6
 *
 * You may choose a different format if you wish as long as it is
 * reasonably readable. Parent, children, and siblings must be
 * clearly distinguishable.
 *
 * /Note/: You can add parameters to functions if you wish, as long
 *  as it doesn't affect the current usage (i.e. tests). This can be
 *  done by adding an optional/default parameter.
 *  E.g.
 *    function prettifyNaryTree<T extends Object>(node: NaryTree<T>): string
 *    function prettifyNaryTree<T extends Object>(node: NaryTree<T>, anotherParameter = ""): string
 */
/**
 * Prettify an Nary tree
 *
 * @param node Root of Nary tree
 * @returns Prettified Nary tree
 */
function prettifyNaryTree(node, depth = 1) {
    const current = node.data.toString();
    const children = node.children.map(each => `${"|- ".repeat(depth)}${prettifyNaryTree(each, depth + 1)}`);
    return [current, ...children].filter(Boolean).join("\n");
}
const prettyNaryTree = prettifyNaryTree(naryTreeExample);
outputText("pretty_ntree_output", prettyNaryTree);
/**
 * For this, we have a series of "base cases" which are the primitive or simple
 * json value types - string, boolean, number, null - which can just be
 * encoded as strings and coverted to the List line format.
 *
 * For more complex types like Array and Object, we will recursively call
 * this function to convert their elements into the required format, add
 * indentation, and wrap with brackets.
 */
const prettifyJson = (json) => {
    return IMPLEMENT_THIS;
};
const json = {
    unit: "FIT2102",
    year: 2021,
    semester: "S2",
    active: true,
    assessments: {
        week1: null,
        week2: "Tutorial 1 Exercise",
        week3: "Tutorial 2 Exercise",
    },
    languages: ["Javascript", "Typescript", "Haskell", "Minizinc"],
};
outputText("pretty_json_output", prettifyJson(json));
