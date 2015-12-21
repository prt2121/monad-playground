function Just(value) {
    this.value = value;
}

Just.prototype.bind = function(transform) {
    return transform(this.value);
};

Just.prototype.toString = function() {
    return 'Just(' +  this.value + ')';
};

var Nothing = {
    bind: function() {
        return this;
    },
    toString: function() {
        return 'Nothing';
    }
};

var result = new Just(5).bind(function(n) {
  return new Just((-1) * Math.log(n));
}).bind(function(n) {
  return Nothing.bind(n);
});

console.log(result.toString());
