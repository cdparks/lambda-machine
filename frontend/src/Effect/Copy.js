// module Effect.Copy
"use strict"

var clipboard = require("clipboard-polyfill/text")

exports.copyImpl = function(Left) {
  return function(Right) {
    return function(text) {
      return function(error, success) {
        clipboard.writeText(text).then(function() {
          success(Right({}))
        }, function(err) {
          error(Left('clipboard.writeText failed: ' + err.toString()))
        })

        return function(_cancel, _cancelError, cancelSuccess) {
          cancelSuccess()
        }
      }
    }
  }
}
