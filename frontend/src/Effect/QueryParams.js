// module Effect.QueryParams
"use strict"

require('url-search-params-polyfill');

exports.getParamImpl = function(Nothing) {
  return function(parse) {
    return function(name) {
      return function() {
        var params = new URLSearchParams(window.location.search)
        var value = params.get(name)
        if (value === null || value === undefined) {
          return Nothing
        }
        return parse(value)
      }
    }
  }
}
