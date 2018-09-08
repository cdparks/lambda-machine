'use strict';

var React = require('react');
var ReactDOM = require('react-dom');

exports.renderToId = function(id) {
  return function(component) {
    return function(props) {
      return function() {
        var element = document.getElementById(id);
        if (!element) {
          throw new Error("No such element: " + id);
        }
        ReactDOM.render(React.createElement(component, props), element);
        return {};
      }
    }
  }
}
