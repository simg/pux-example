"use strict";


exports.parseDateStringImpl = function (just, nothing, dateString) {
  var t = new Date(dateString);
  return isNaN(t) ? nothing : just(t.getTime());
};

/*
exports.parseDateStringImpl = function (just) {
  return function (nothing) {
    return function (dateString) {
      var t = new Date(dateString)
      return isNaN(t) ? nothing : just(t);
    };
  };
};*/

/*
exports.parse = function (dateString) {
  return function () {
    var d = new Date(dateString);
    if (!isNaN(d)) {
      return Data_Tuple.Tuple(d.getTime())(d.getTimeZoneOffset())
    } else {

    }
  };
};
*/