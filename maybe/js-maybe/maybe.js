Maybe = function(value) {
  var Nothing = {
    bind: function(fn) {
      return this;
    },
    isNothing: function() {
      return true;
    },
    val: function() {
      throw new Error("cannot call val() nothing");
    },
    maybe: function(def, fn) {
      return def;
    }
  };

  var Something = function(value) {
    return {
      bind: function(fn) {
        return Maybe(fn.call(this, value));
      },
      isNothing: function() {
        return false;
      },
      val: function() {
        return value;
      },
      maybe: function(def, fn) {
        return fn.call(this, value);
      }
    };
  };

  if (typeof value === 'undefined' || value === null || isNaN(value))
    return Nothing;

  return Something(value);
};

console.log(Maybe(100).bind(function(n) {
  return (-1) * Math.log(n);
}).bind(function(n) {
  return Math.log(n);
}).isNothing());
