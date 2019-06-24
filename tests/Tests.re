
let one = {"one": {"two": {"three": 4}}};

let two = [%js.deep one["one"]["two"]["three"].replace(5)];
/* Fails type check of arg
let two = [%js.deep one["one"]["two"]["three"].replace(5.0)];
*/

let three = [%js.deep one["one"]["two"]["three"].map(n => n + 10)];
/* Fails type check of transform function
let two = [%js.deep one["one"]["two"]["three"].map(n => n +. 1.0)];
*/

/* Fails type check for path
let two = [%js.deep one["one"]["two"]["five"].replace(5)];
let two = [%js.deep one["one"]["three"]["five"].replace(5)];
*/

assert(two == {"one": {"two": {"three": 5}}});
Js.log2("ok!", two)

let other = {"one": {"two": {"three": {"four": 4, "five": 5}}}};

let other_new = [%js.deep other["one"]["two"]["three"].map(three => three["four"].replace(10)["five"].replace(50))];
assert(other_new == {"one": {"two": {"three": {"four": 10, "five": 50}}}})