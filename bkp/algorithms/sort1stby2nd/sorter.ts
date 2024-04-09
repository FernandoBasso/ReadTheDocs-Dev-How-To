let xs = [
  { id: 1, name: "One" },
  { id: 2, name: "Two" },
  { id: 3, name: "Three" },
  { id: 4, name: "Four" },
];

let ys = [
  { id: 4, name: "Four" },
  { id: 2, name: "Two" },
];

function customSort(array: any[], order: any[]): any[] {
  return order.map((i) => array.find((e) => e.id === i));
}

log(customSort(xs, [4, 2, 1, 3]));

var arr = ["one", "four", "two"];
var test = [{ key: "one" }, { key: "two" }, { key: "four" }];

function sortFunction(a, b) {
  var indexA = arr.indexOf(a["key"]);
  var indexB = arr.indexOf(b["key"]);
  if (indexA < indexB) {
    return -1;
  } else if (indexA > indexB) {
    return 1;
  } else {
    return 0;
  }
}
