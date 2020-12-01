const MENUSTATE_INIT = 0;
const MENUSTATE_REGISTER = 1;
const MENUSTATE_LOGIN = 2;

var currentState = -1;

function onLoad() {
    (async () => {
        await import('/static/axlsign.js');
        currentState = -1;
        setMenuState(MENUSTATE_INIT);
    })();
}

function registerMenuSelected() {
    setMenuState(MENUSTATE_REGISTER);
}

function loginMenuSelected() {
    setMenuState(MENUSTATE_LOGIN);
}

function registerValidation(email, name, pass1, pass2) {
    var fail = false;

    clearValidators();

    if ("" == email) {
        setSpan("validateEmail1", "Valid email required.");
        fail = true;
    }

    if ("" == name) {
        setSpan("validateName", "Valid name required.");
        fail = true;
    }

    if ("" == pass1) {
        setSpan("validatePassword1", "Password can't be blank.");
        fail = true;
    }

    if ("" == pass2) {
        setSpan("validatePassword2", "Password can't be blank.");
        fail = true;
    }

    if (pass1 != pass2) {
        setSpan("validatePassword1", "Passwords must match.");
        setSpan("validatePassword2", "Passwords must match.");
        fail = true;
    }

    if (fail) {
        setSpan("validateRegistration", "Fix validation errors below.");
        return false;
    } else {
        return true;
    }
}

function createRandomSalt() {
    var salt = new Uint8Array(32);
    crypto.getRandomValues(salt);

    return salt;
}

function createRandomIV() {
    var iv = new Uint8Array(16);
    crypto.getRandomValues(iv);

    return iv;
}

function createKeysFromPassword(salt, password) {
    var enc = new TextEncoder();
    var convertedPassword = enc.encode(password);

    return
        (async(salt, convertedPassword) => {
            var importedKey =
                await crypto.subtle.importKey(
                    "raw", convertedPassword, {name: "PBKDF2"}, false,
                    ["deriveKey", "deriveBits"]);

            var secretKey =
                await crypto.subtle.deriveKey(
                    {"name": "PBKDF2", "salt": salt, "iterations": 5000,
                     "hash": "SHA-384"},
                    importedKey,
                    {"name": "AES-CBC", "length": 256}, false,
                    ["encrypt", "decrypt"]);

            var hmacSecretKey =
                await crypto.subtle.deriveKey(
                    {"name": "PBKDF2", "salt": salt, "iterations": 5000,
                     "hash": "SHA-384"},
                    importedKey,
                    {"name": "HMAC", "hash": "SHA-384"}, false,
                    ["sign", "verify"]);

            return {"encryptKey" : secretKey, "hmacKey" : hmacSecretKey};
        })(salt, convertedPassword);
}

function register() {
    var email = getValue("regemail");
    var name = getValue("name");
    var pass1 = getValue("password1");
    var pass2 = getValue("password2");

    if (!registerValidation(email, name, pass1, pass2)) {
        return;
    }

    (async (email, name, pass1) => {
        var salt = createRandomSalt();
        var keys = await createKeysFromPassword(salt, pass1);
        var encryptKey = keys.encryptKey;
        var hmacKey = keys.hmacKey;
        var keypair = axlsign.generateKeyPair(createRandomSalt());

        var encObj = { "keypair" : keypair };
        var encString = JSON.stringify(encObj);

        var enc = new TextEncoder();
        var convEncData = enc.encode(encString);
        var iv = createRandomIV();
        var encData =
            await crypto.subtle.encrypt(
                {"name" : "AES-CBC", "iv" : iv}, encryptKey, convEncData);
        var encDataBytes = new Uint8Array(encData);
        var signature =
            await crypto.subtle.sign("HMAC", hmacKey, encData);
        var signatureBytes = new Uint8Array(signature);

        var obj = {
            "email" : email,
            "name" : name,
            "salt" : salt,
            "pubkey" : keypair.public,
            "iv" : iv,
            "encdata" : encDataBytes,
            "mac" : signatureBytes };

        var objStr = JSON.stringify(obj);

        /* TODO - send objStr to user registration. */

        setMenuState(-1);
    })(email, name, pass1);
}

function loginValidation(email, password) {
    var fail = false;

    clearValidators();

    if ("" == email) {
        setSpan("validateEmail", "Valid email required.");
        fail = true;
    }

    if ("" == password) {
        setSpan("validatePassword", "Valid password required.");
        fail = true;
    }

    if (fail) {
        setSpan("validateLogin", "Fix validation errors below.");
        return false;
    } else {
        return true;
    }
}

function login() {
    var email = getValue("email");
    var password = getValue("password");

    if (!loginValidation(email, password)) {
        return;
    }

    setMenuState(-1);
}

function setMenuState(state) {
    startMenuUndoState(currentState);
    registerMenuUndoState(currentState);
    loginMenuUndoState(currentState);

    switch (state) {
        case MENUSTATE_REGISTER:
            currentState = MENUSTATE_REGISTER;
            break;

        case MENUSTATE_LOGIN:
            currentState = MENUSTATE_LOGIN;
            break;

        default:
            currentState = MENUSTATE_INIT;
            break;
    }

    startMenuSetState(currentState);
    registerMenuSetState(currentState);
    loginMenuSetState(currentState);
}

function setClass(element, classname) {
    document.getElementById(element).className = classname;
}

function startMenuUndoState(state) {
    switch (state) {
        case MENUSTATE_INIT:
        default:
            setClass("startMenu", "hide");
            clearValidators();
            break;
    }
}

function startMenuSetState(state) {
    switch (state) {
        case MENUSTATE_INIT:
            setClass("startMenu", "show");
            break;
    }
}

function registerMenuUndoState(state) {
    switch (state) {
        case MENUSTATE_REGISTER:
            setClass("registerMenu", "hide");
            clearValidators();
            break;
    }
}

function registerMenuSetState(state) {
    switch (state) {
        case MENUSTATE_REGISTER:
            setClass("registerMenu", "show");
            break;
    }
}

function loginMenuUndoState(state) {
    switch (state) {
        case MENUSTATE_LOGIN:
            setClass("loginMenu", "hide");
            clearValidators();
            break;
    }
}

function loginMenuSetState(state) {
    switch (state) {
        case MENUSTATE_LOGIN:
            setClass("loginMenu", "show");
            break;
    }
}

function clearSpan(id) {
    setSpan(id, "");
}

function setSpan(id, value) {
    document.getElementById(id).innerHTML = value;
}

function getValue(id) {
    return document.getElementById(id).value;
}

function clearValidators() {
    clearSpan("validateRegistration");
    clearSpan("validateEmail1");
    clearSpan("validateName");
    clearSpan("validatePassword1");
    clearSpan("validatePassword2");
    clearSpan("validateLogin");
    clearSpan("validateEmail");
    clearSpan("validatePassword");
}
