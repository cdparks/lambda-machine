// module Effect.DOM
"use strict";

exports.getBody = function() {
  return assertDefined(
    window.document.body,
    'window.document.body'
  )
}

exports.getRoot = function() {
  return assertDefined(
    window.document.getElementById('root'),
    'root container element'
  )
}

exports.getPortal = function() {
  return assertDefined(
    window.document.getElementById('portal'),
    'portal container element'
  )
}

function assertDefined(value, name) {
  if (value === null || value === undefined) {
    throw new Error(name + ' not defined')
  }
  return value
}
