// module Effect.Save
"use strict"

exports.saveImpl = function(Left) {
  return function(Right) {
    return function(text) {
      return function(filename) {
        return function() {
          var blob = new Blob([text], {type: 'text/plain;charset=utf-8'})
          try {
            require('file-saver').saveAs(blob, filename)
            return Right({})
          } catch (e) {
            return Left(e.toString())
          }
        }
      }
    }
  }
}
