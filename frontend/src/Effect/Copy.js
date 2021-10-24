// module Effect.Copy
"use strict";

exports.hasClipboard = navigator && 'clipboard' in navigator

exports.clipboardWriteImpl = function(Left) {
  return function(Right) {
    return function(text) {
      return function(error, success) {
        var promise = (
          'writeText' in navigator.clipboard
            ? navigator.clipboard.writeText(text)
            : navigator.clipboard.write([clipboardTextItem(text)])
        )

        promise.then(function() {
          success(Right({}))
        }, function(err) {
          error(Left(err.toString()))
        })

        return function(_cancel, _cancelError, cancelSuccess) {
          cancelSuccess()
        }
      }
    }
  }
}

function clipboardTextItem(text) {
  var blob = new Blob([text], {type: 'text/plain'})
  return new ClipboardItem({'text/plain': blob})
}
