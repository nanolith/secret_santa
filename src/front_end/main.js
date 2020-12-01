const MENUSTATE_INIT = 0;
const MENUSTATE_REGISTER = 1;
const MENUSTATE_LOGIN = 2;

var currentState = -1;

function onLoad() {
    currentState = -1;
    setMenuState(MENUSTATE_INIT);
}

function registerMenuSelected() {
    setMenuState(MENUSTATE_REGISTER);
}

function loginMenuSelected() {
    setMenuState(MENUSTATE_LOGIN);
}

function register() {
    var email = getValue("regemail");
    var name = getValue("name");
    var pass1 = getValue("password1");
    var pass2 = getValue("password2");
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
        return;
    }

    setMenuState(-1);
}

function login() {
    var email = getValue("email");
    var password = getValue("password");
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
