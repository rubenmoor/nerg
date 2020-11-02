// module Drawing

exports.nestedFor =
  function(bottom) {
    return function(top) {
      return function(left) {
        return function(right) {
          return function(acc) {
            return function() {
              for(var row = bottom; row <= top; row++) {
                for(var col = left; col <= right; col++) {
                  acc(row)(col)();
                }
              }
              return [];
            };
          };
        };
      };
    };
  };
