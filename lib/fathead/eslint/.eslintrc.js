module.exports = {
    "env": {
        "es6": true,
        "node": true,
        "jasmine": true
    },
    "extends": "eslint:recommended",
    "globals": {
    },
    "rules": {
        "brace-style": ["error",
            "1tbs", {
                "allowSingleLine": true
            }
        ],
        "dot-location": [
            "error",
            "property"
        ],
        "func-style": [
            "error",
            "declaration"
        ],
        "indent": [
            "error",
            4
        ],
        "linebreak-style": [
            "error",
            "unix"
        ],
        "lines-around-comment": [
            "error", {
                "beforeLineComment": true,
                "allowBlockStart": true
            }
        ],
        "no-lonely-if": "error",
        "no-trailing-spaces": "error",
        "one-var": [
            "error",
            "never"
        ],
        "padded-blocks": [
            "error",
            "never"
        ],
        "quotes": [
            "error",
            "single"
        ],
        "require-jsdoc": [
            "error", {
                "require": {
                    "FunctionDeclaration": true,
                    "MethodDefinition": true,
                    "ClassDeclaration": true
                }
            }
        ],
        "semi": [
            "error",
            "always"
        ],
        "space-before-function-paren": [
            "error",
            "never"
        ],
        "strict": [
            "error",
            "global"
        ],
        "valid-jsdoc": "error"
    }
};
