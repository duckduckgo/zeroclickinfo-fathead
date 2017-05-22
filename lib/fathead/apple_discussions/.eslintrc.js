module.exports = {
    "env": {
        "node": true,
		"es6": true
    },
    "extends": "eslint:recommended",
    "parserOptions": {
        "sourceType": "module",
		"ecmaVersion": 8
    },
    "rules": {
        "indent": [
            "error",
            "tab"
        ],
        "linebreak-style": [
            "error",
            "windows"
        ],
        "semi": [
            "error",
            "always"
        ]
    }
};
