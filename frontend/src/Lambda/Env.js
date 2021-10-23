// module Lambda.Env
"use strict";

exports.api = process.env.API || "https://api.lambda-machine.com";
exports.host = process.env.HOST  || "https://lambda-machine.com"
