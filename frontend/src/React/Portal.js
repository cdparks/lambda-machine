// module React.Portal
"use strict"

exports.createPortal = function(jsx) {
  return function(element) {
    return require('react-dom').createPortal(jsx, element)
  }
}
