
function apply(h, f) {
  return function(w) {
    return f(h(w))(w);
  };
}

function ret(x) {
  return function(_){
    return x;
  };
}

function prod2(n) {
  return n * 2;
}

function plus10(n) {
  return n + 10;
}

var addStuff = apply(prod2, function(a) {
  console.log(a);
  return apply(plus10, function(b) {
    return ret(a + b);
  });
});

console.log(addStuff(3));
