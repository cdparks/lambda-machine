// module React.Portal
"use strict";

var ReactDOM = require('react-dom')

exports.createPortal = function(jsx) {
  return function(element) {
    return ReactDOM.createPortal(jsx, element)
  }
}
