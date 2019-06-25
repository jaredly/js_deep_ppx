# [@js.deep] for immutably updating javascript objects in Reason/OCaml

Motivation: Reason has this nice syntax for constructing javascript objects `{"some": thing}`, and a nice way to represent the types `type t = {. "some": int}`, and so it can be quite convenient when working with external javascript libraries to just keep everything in the original data format ... that is, until you want to update something. Mutative updates require creating `external`s for every field, and trying to update things immutably is an exercise in frustration.

That's where `js.deep` comes in! This ppx allows you to retain type safety, while making immutable updates to deeply nested javascript objects almost painless.

## Usage:

`js.deep` transforms code that looks like `some_value["attrname"].replace(newvalue)` into code that does the equivalent of the javascript `{...some_value, attrname: newvalue}`, and `some_value["attrname"].map(somefn)` with `{...some_value, attrname: somefn(some_value.attrname)}`.

```reason
// Some nested data
let one = {"one": {"two": {"three": 4}}};

// Updating a field -- this replaces the value at "three" with 5
let two = [%js.deep one["one"]["two"]["three"].replace(5)];
/* Fails type check (arg needs to be an int)
let two = [%js.deep one["one"]["two"]["three"].replace(5.0)];
*/

assert(two == {"one": {"two": {"three": 5}}});

// Updating a field with a map function, increasing the value by 10
let three = [%js.deep one["one"]["two"]["three"].map(n => n + 10)];
/* Fails type check (transform function expects a float, gets an int)
let two = [%js.deep one["one"]["two"]["three"].map(n => n +. 1.0)];
*/

assert(three == {"one": {"two": {"three": 14}}});

/* Fails type check (accessing paths that don't exist)
let two = [%js.deep one["one"]["two"]["five"].replace(5)];
let two = [%js.deep one["one"]["three"]["five"].replace(5)];
*/

let other = {"one": {"two": {"three": {"four": 4, "five": 5}}}};

// map + replace!
let other_new = [%js.deep other["one"]["two"]["three"].map(
  // update two fields
  three => three["four"].replace(10)["five"].replace(50)
)];
assert(other_new == {"one": {"two": {"three": {"four": 10, "five": 50}}}})
```

## Installation

`npm i js_deep_ppx`

bsconfig.json

```
"ppx-flags": ["js_deep_ppx/ppx"]
```

## Operators
This ppx includes two operators, that are valid within the `[%get_in ]` form.

- `#??` is to be used when *both sides* are optional. E.g. the object on the left is optional, and the attribute you're getting out is also optional. e.g. `option({. "attr": option(int)})`
- `#?` is to be used when *only the object* is optional, but the attribute you're getting out is not. e.g. `option({. "attr": int})`

## Usage

```
/* some data with optional attributes in javascript objects (e.g. from graphql) */
let one = Some({"two": Some({"three": 4})});

let x: option(int) = [%get_in one#??two#?three];
```


