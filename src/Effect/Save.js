// module Effect.Save

exports.saveTextAs = function(text) {
  return function(filename) {
    return function() {
      var blob = new Blob([text], {type: 'text/plain;charset=utf-8'});
      saveAs(blob, filename);
      return {};
    }
  }
}

