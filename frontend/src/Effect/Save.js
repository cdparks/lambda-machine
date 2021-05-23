// module Effect.Save
"use strict";

exports.saveTextAs = function(text) {
  return function(filename) {
    return function() {
      var blob = new Blob([text], {type: 'text/plain;charset=utf-8'});
      require('file-saver').saveAs(blob, filename);
      return {};
    }
  }
}

