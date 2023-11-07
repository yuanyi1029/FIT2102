/**
 * Lambda calculus
 *
 * Refer to the README and web page for instructions.
 */

import { from, fromEvent } from "rxjs";
import { map, filter, mergeMap } from "rxjs/operators";

fromEvent(document, "click").pipe(
    mergeMap
)