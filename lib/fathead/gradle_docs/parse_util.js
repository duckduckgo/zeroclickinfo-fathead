'use strict';

const promise = require("promise");
const request = require("request");

const requestWrapper = function(url, json){
				        json = json || false;
				        return new promise(function (resolve, reject) {
				            request({url:url, json:json}, function (err, res, body) {
				                if (err) {
				                    return reject(err);
				                } else if (res.statusCode !== 200) {
				                    err = new Error("Unexpected status code: " + res.statusCode);
				                    err.res = res;
				                    return reject(err);
				                }
				                resolve(body);
				                });
				            });
    					}

const isTextNode = function(){
            		// If this is a text node, return true.
            		return(this.nodeType === 3);
    			}

module.exports = {
	requestWrapper: requestWrapper,
	isTextNode: isTextNode
};