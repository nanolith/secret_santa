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
    setMenuState(-1);
}

function login() {
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
