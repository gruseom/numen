var vm = require('vm');
var fs = require('fs');
var net = require('net');

global.D = v8debug.Debug; // the V8 debugger
var S = null; // server
var C = null; // client connection

global.require = require;
global.exports = {};
global.module = {};
clearAllBreakpoints(); // clear breakpoint that gets set on startup

// responses

function response (req) {
    var res = null;
    if (req.evaluate) {
        res = { 'evaluation' : evaluateAtTopLevel(req.evaluate, req.break_) }
    } else if (req.needs) {
        res = { 'id' : req.id, 'supplement' : fillNeeds(req.id, req.needs) }
    } else if (req.load) {
        res = { 'loaded' : numenLoad(req.load, req.breakpoints) };
    } else if (req.compile) {
        res = { 'compiled' : compile(expand(read_string(req.compile))) };
    } else if (req.source) {
        res = { 'script' : req.source, 'source' : source(req.source) };
    } else if (req.breakpoint) {
        res = setBreakpoint(req.breakpoint.script, req.breakpoint.line, req.breakpoint.state);
        res.breakpoints = allBreakpoints();
    } else if (typeof req.breakpoints !== 'undefined') {
        res = ensureBreakpointsOn(req.breakpoints || {});
    } else if (req.toggle_break_on_exception) {
        res = { 'status-message' : toggleBreakOnException() };
    } else if (req.clear_all_breakpoints) {
        clearAllBreakpoints();
        res = { 'status-message' : 'Breakpoints cleared'  };
    } else if (req.client_callback != null) {
        clientCallback(req.client_callback, req.errmsg, req.output);
    } else if (req.run || req.locals) {
        res = { 'status-message' : 'Server is not in the debugger' };
    } else {
        throw new Error('Unrecognized request ' + JSON.stringify(req));
    }

    if (res && req.stuff) { res.stuff = req.stuff; } // pass any client stuff back
    return res;
}

var breakOnLoad = null;
var exceptionInProgress = null;

function debugListener (event, exec_state, event_data, data) {
    if (event === D.DebugEvent.Exception) {
        var e = event_data.exception_;
        if (e === exceptionInProgress) { // don't break twice for the same exception
            exceptionInProgress = null;
        } else {
            exceptionInProgress = e;
            enterDebugger(exec_state, e);
        }
    } else if (event !== D.DebugEvent.Break) {
        // we're not handling any other kind of event
    } else if (breakOnLoad) {
        // we asked for a break event while loading a script in order
        // to e.g. set some breakpoints. call the callback and leave.
        breakOnLoad();
        breakOnLoad = null;
    } else {
        enterDebugger(exec_state);
    }
}

D.setListener(debugListener);

var scriptID = 0;
function uniqueEvalScriptName () {
    return 'eval' + ++scriptID;
}

global.$ = {};
var maxStoredEvals = 10;
var evalID = -1;

function clientValue (val, depth, batch) {
    evalID++;
    $[evalID] = val;
    delete $[evalID - maxStoredEvals];
    return { 'value' : represent(val, null, depth, batch), 'id' : evalID };
}

var runningLumen = false;

function evaluateAtTopLevel (code, breakAtStart) {
    if (breakAtStart) {
        D.breakExecution();
    }
    if (runningLumen) {
        return clientValue(eval(read_string(code)));
    } else {
        return clientValue(vm.runInThisContext(code, uniqueEvalScriptName()));
    }
}

function fillNeeds (id, needs) {
    if (!$[id]) { return 'deleted'; }
    for (var i=0; i<needs.length; i++) {
        var path = parseVkey(needs[i].vkey);
        needs[i].details = represent(findObj(path), needs[i].from, null, needs[i].batch);
    }
    return needs;
}

global.numenLoad = function (script, breakpoints, asName, exitCodeIfFail) { // numen-load
    var code = fs.readFileSync(script).toString();
    if (breakpoints) {
        breakOnLoad = function () { ensureBreakpointsOn(breakpoints); }
        D.breakExecution();
    }
    try {
        vm.runInThisContext(code, asName || script);
    } catch (e) {
        sendException(e);
        if (exitCodeIfFail) {
            process.exit(exitCodeIfFail);
        }
    }
    return script;
}

function source (script) {
    var scriptObj = D.findScript(script);
    return scriptObj != null ? scriptObj.source : null;
}

function sendException (e) {
    try {
        clientSend({ 'evaluation' : clientValue(e) });
        numenLogFile(errorToString(e));
    } catch (e2) {
        log(errorToString(e));
        log("While trying to send that error, another error occurred:");
        log(errorToString(e2));
    }
}

// breakpoints

function setBreakpoint (script, line, state) {
    state = state || "on";
    var scriptObj = D.findScript(script);
    if (!scriptObj) {
        return { 'failure' : "Can't find loaded script " + script };
    }
    var bp = findBreakpoint(scriptObj, line);
    if (!bp && state === "on") {
        bp = D.setScriptBreakPointById(scriptObj.id, line);
        if (!findBreakpoint(scriptObj, line)) {
            return { 'failure' : 'Failed to set breakpoint at line ' + line };
        }
        return { 'status' : 'now-on' };
    } else if (bp && state === "off") {
        D.clearBreakPoint(bp.number());
        if (findBreakpoint(scriptObj, line)) {
            return { 'failure' : 'Failed to clear breakpoint at line ' + line };
        }
        return { 'status' : 'now-off' };
    } else if (bp && state === "on") {
        return { 'status' : 'already-on' };
    } else if (!bp && state === "off") {
        return { 'status' : 'already-off' };
    } else {
        throw new Error('Invalid breakpoint request');
    }
}

function ensureBreakpointsOn (breakpoints) {
    for (script in breakpoints) {
        var lines = breakpoints[script];
        for (var i=0; i< lines.length; i++) {
            setBreakpoint(script, lines[i]);
        }
    }
}

function findBreakpoint (scriptObj, line) {
    var bps = v8debug.GetScriptBreakPoints(scriptObj);
    for (var i=0; i<bps.length; i++) {
        if (bps[i].line() === line) {
            return bps[i];
        }
    }
}

global.allBreakpoints = function () {
    var bps = [];
    var v8bps = v8debug.script_break_points;
    for (var i=0; i < v8bps.length; i++) {
        var scriptObj = findScriptByID(v8bps[i].script_id());
        if (scriptObj != null) {
            bps.push({ 'script' : scriptObj.name, 'line' : v8bps[i].line(),
                       'text' : trim(scriptObj.sourceLine(v8bps[i].line())) });
        }
    }
    return bps;
}

function clearAllBreakpoints () {
    // curiously, Debug.clearAllBreakPoints doesn't get them all
    var v8bps = v8debug.script_break_points;
    for (var i=0; i<v8bps.length; i++) {
        D.clearBreakPoint(v8bps[i].number());
    }
}

function findScriptByID (script_id) {
    var scripts = D.scripts();
    for (var i in scripts) {
        var script = scripts[i];
        if (script_id === script.id) {
            return script;
        }
    }
}

function toggleBreakOnException () {
    D.isBreakOnException() ? D.clearBreakOnException() : D.setBreakOnException();
    return 'Break on exception is ' + (D.isBreakOnException() ? 'on' : 'off');
}

// debugger

function enterDebugger (exec_state, e) {
    var frames = numenFrames(exec_state);
    var res = (frames.length > 0) ? { 'break' : frames } : null;
    if (e) {
        var obj = clientValue(e);
        obj.breaking = true;
        res = res || {};
        res.e = obj;
    }
    if (res) {
        clientSend(res);
        runDebuggerLoop(exec_state);
    }
}

function waitForDebuggerCommand () {
    var str = "";
    var fd = C.in_.fd;
    while (true) {
        str += fs.readSync(fd, 256)[0];
        if (0 === fs.fstatSync(fd).size) {
            break;
        }
    }
    return str ? JSON.parse(str) : null;
}

function runDebuggerLoop (exec_state) {
    var exit = false;
    while (!exit) {
        try {
            var res = null;
            // The debugger loop must block until a debug command is
            // issued. If we tried to read asynchronously, this
            // function would return immediately, in which case the
            // debugger would always "continue" before the client ever
            // knew that a breakpoint had been hit.
            var req = waitForDebuggerCommand();
            if (req) {
                var frame = req.frame_index != null ? v8Frame(exec_state, req.frame_index) : null;
                if (req.run) {
                    // 'continue' translates to null action
                    if (D.StepAction[req.run] != null) {
                        exec_state.prepareStep(D.StepAction[req.run], 1);
                    }
                    // leave the debugger. rely on v8 to break again after step, if applicable.
                    res = { 'resuming' : true };
                    exit = true;
                } else if (req.locals) {
                    var obj = clientValue(locals(frame));
                    obj.value.locals = true;
                    res = { 'evaluation' : obj };
                } else if (req.evaluate) {
                    res = { 'evaluation' : clientValue(frame.evaluate(req.evaluate).value()) };
                }
                // handle other commands by replying but staying in debugger
                if (!res) {
                    res = response(req);
                }

                if (res) {
                    if (req.stuff) { res.stuff = req.stuff; } // pass any client stuff back
                    clientSend(res);
                }
            }
        }
        catch (e) {
            sendException(e);
        }
    }
}

function numenFrames (exec_state) {
    var frames = [];
    for (var i=0; i<exec_state.frameCount(); i++) {
        var frame = exec_state.frame(i);
        var fn = frame.func();
        var script = fn.script().name();
        frames.push({ 'script' : script, 'fn' : fn.name(),
                      'line' : frame.sourceLine(), 'column' : frame.sourceColumn(),
                      'text' : trim(frame.sourceLineText()) });
    }
    return frames;
}

function locals (v8FrameMirror) {
    var obj = {};
    // the real goods are hidden inside a FrameDetails inside a FrameMirror
    var frm = v8FrameMirror.details_.details_;
    var argStart = v8debug.kFrameDetailsFirstDynamicIndex;
    var argCount = frm[v8debug.kFrameDetailsArgumentCountIndex];
    addLocals(obj, frm, argStart, argCount);
    var localCount = frm[v8debug.kFrameDetailsLocalCountIndex];
    var localStart = argStart + argCount * v8debug.kFrameDetailsNameValueSize;
    addLocals(obj, frm, localStart, localCount);
    return obj;
}

function addLocals (obj, frm, start, count) {
    for (var i=0, k=start; i<count; i++, k += v8debug.kFrameDetailsNameValueSize) {
        var name = frm[k + v8debug.kFrameDetailsNameIndex];
        var val = frm[k + v8debug.kFrameDetailsValueIndex];
        obj[name] = val;
    }
}

function v8Frame (exec_state, n) {
    if (typeof n !== 'number' && n < 0 || n >= exec_state.frameCount()) {
        throw new Error(n + ' is not a valid frame');
    }
    return exec_state.frame(n);
}

// semantic output

var defaultChars = 1000;
global.numenDefaultLength = 10;
global.numenDefaultDepth = 10;

function represent (value, from, depth, batch) {
    from = from || 0;
    depth = (depth == null) ? numenDefaultDepth : depth;
    if (value == null) {
        return { 'null' : (typeof value === 'undefined') ? 'undefined' : 'null' }
    }
    if (typeof value === 'boolean') { return { 'bool' : value.toString() } }
    if (typeof value === 'number') { return { 'num' : value.toString() } }
    if (value instanceof Date) { return { 'date' : value.toISOString() } }
    if (typeof value === 'function') {
        var obj = { 'fn' : value.name || 'fn' }
        var mirror = D.MakeMirror(value);
        var scriptObj = mirror.script();
        if (scriptObj != null) {
            obj.script = scriptObj.name();
        }
        var loc = mirror.sourceLocation();
        if (loc != null) {
            obj.line = loc.line;
            obj.column = loc.column;
        }
        return obj;
    }
    if (typeof value === 'string') {
        return { 'str' : value.substr(from, batch || defaultChars), 'truelen' : value.length }
    }
    if (typeof value !== 'object') {
        throw new Error('Unrecognized type ' + typeof value);
    }
    if (value instanceof ErrorMessage) {
        var obj = represent(value.str, from);
        obj['errmsg?'] = true;
        return obj;
    }
    if (value instanceof ScriptLocation) {
        value['loc?'] = true;
        return value;
    }
    if (value instanceof Error) {
        var trace = substitute(value); // stash our own parsed error object
        if (from == 0 && batch == null) { batch = 2 }
        var obj = represent(trace, from, null, batch);
        obj['error?'] = true;
        return obj;
    }

    // too deep? give them stubs
    if (depth <= 0 && !trivial(value)) {
        return treatAsArray(value) ? { 'vals' : [], 'truelen' : value.length }
        : { 'keys' : [], 'vals' : [], 'truelen' : Object.getOwnPropertyNames(value).length };
    }

    if (treatAsArray(value)) {
        var to = Math.min(value.length, from + (batch || numenDefaultLength));
        var vals = new Array(to - from);
        for (var i = from; i < to; i++) {
            vals[i-from] = represent(value[i], null, depth-1);
        }
        return { 'vals' : vals, 'truelen' : value.length };
    }

    var allKeys = Object.getOwnPropertyNames(value);
    if (allKeys.length <= 10000) {
        allKeys.sort(keyOrder);
    }
    var to = Math.min(allKeys.length, from + (batch || numenDefaultLength));
    var keys = new Array(to - from);
    var vals = new Array(to - from);
    for (var i = from; i < to; i++) {
        keys[i-from] = allKeys[i];
        vals[i-from] = represent(value[allKeys[i]], null, depth-1);
    }
    return { 'keys' : keys, 'vals' : vals, 'truelen' : allKeys.length };
}

// sending requests to client

var clientCallID = 0;
var clientCallbacks = {};

function clientCallback (id, errmsg, output) {
    var callback = clientCallbacks[id];
    if (callback) {
        callback(errmsg ? new Error(errmsg) : null, output);
        delete callback[id];
    } else {
        throw new Error('No client callback with id ' + id);
    }
}

// api

global.numenWriteInPlace = function (text, preserve) { // numen-write-in-place
    clientSend({ 'in-place' : text, 'preserve-p' : preserve ? true : null });
}

global.numenReportValue = function (val, depth, batch) { // numen-report-value
    clientSend({ 'evaluation' : clientValue(val, depth, batch) });
}

global.numenSlimeEval = function (callback, string, package) { // numen-slime-eval
    var id = clientCallID++;
    clientCallbacks[id] = callback;
    clientSend({ 'slime-eval' : string, 'callback-id' : id, 'package' : package });
}

global.numenEmacsEval = function (callback, string) { // numen-emacs-eval
    var id = clientCallID++;
    clientCallbacks[id] = callback;
    clientSend({ 'emacs-eval' : string, 'callback-id' : id });
}

// utilities

function ErrorMessage (str) { this.str = trim(str); }

function ScriptLocation (fname, script, line, column) {
    this.fn = fname;
    this.script = script;
    this.line = line;
    this.column = column;
    var scriptObj = D.findScript(script);
    this.text = (scriptObj ? trim(scriptObj.sourceLine(line)) : null) || "";
}

function v8StackTraceAsNumenArray (stack) {
    try {
        var arr = stack.split("   at ");
        arr[0] = new ErrorMessage(arr[0]);
        for (var i=1; i<arr.length; i++) {
            arr[i] = trim(arr[i]);
            if (arr[i].indexOf('(') < 0) {
                arr[i] = "<anonymous> (" + arr[i] + ")";
            }
            // match foo, bar in "foo (bar)"
            var fname = "<unknown>", script = "<unknown>", line = null, column = null;
            var res1 = /([^ ]+) \(([^)]*)\)/.exec(arr[i]);
            if (res1 != null) {
                fname = res1[1];
                script = res1[2];
                // match baz, 1, 2 in "baz:1:2"
                var res2 = /([^:]+):([0-9]+):([0-9]+)/.exec(script);
                if (res2 != null) {
                    script = res2[1];
                    line = parseInt(res2[2]) - 1; // convert to zero-based
                    column = parseInt(res2[3]);
                }
            }
            arr[i] = new ScriptLocation(fname, script, line, column);
        }
        return arr;
    } catch (e) {
        log('Numen failed on the following stack trace...');
        log(stack);
        log('...because this happened while trying to parse it:');
        log(e.stack);
    }
}

function errorToString (e) {
    return e.stack || ((e.name || "Error") + ": " + e.message);
}

function errorToNumenTrace (e) {
    if (e.stack) {
        return v8StackTraceAsNumenArray(e.stack);
    } else if (e.name && e.message) {
        return [e.name + ": " + e.message];
    } else if (e.name || e.message || e.type) {
        return [e.message || e.name || e.type];
    } else {
        return ["Unknown error"];
    }
}

function parseVkey (vkey) {
    var rest = vkey;
    var path = null;
    var res = /\$\[([0-9]+)\]/.exec(rest); // match 99 in "$[99]"
    var id = parseInt(res[1]);
    if (!isNaN(id)) {
        path = [id];
        rest = rest.substr(res[0].length);
    }
    var reg = /\.([^[.]+)|\[([0-9]+)\]/; // match abc in ".abc" or 33 in "[33]"
    var prev = (path || []).length;
    while (path && rest) {
        if (res = reg.exec(rest)) {
            if (res[1] != null) {
                path.push(res[1]);
            } else if (res[2] != null) {
                var num = parseInt(res[2]);
                if (!isNaN(num)) {
                    path.push(num);
                }
            }
            rest = rest.substr(res[0].length);
        }
        if (prev === path.length) {
            path = null;
        } else {
            prev = path.length;
        }
    }

    if (!path) {
        throw new Error('Invalid value key: ' + vkey);
    }
    return path;
}

function substitute (obj) {
    if (obj instanceof Error) {
        return obj.numenTrace || (obj.numenTrace = errorToNumenTrace(obj));
    }
}

function findObj (path) {
    var obj = $;
    for (var i=0; i<path.length; i++) {
        obj = obj[path[i]];
        obj = substitute(obj) || obj; // some things we stash our own edition of
        if (!obj) {
            throw new Error('Eval result ' + path[0] + ' has nothing at path [' + path.slice(0,i+1) + '].');
        }
    }
    return obj;
}

function keyOrder (a, b) {
    a = a.toString().toLowerCase();
    b = b.toString().toLowerCase();
    return a < b ? -1 : a > b ? 1 : 0;
}

function trivial (val) {
    if (treatAsArray(val)) {
        if (val.length > 2) { return false; }
        for (var i=0; i<val.length; i++) {
            if (val[i] && typeof val[i] === 'object') { return false; }
        }
    } else {
        var len = 0;
        for (k in val) {
            if (++len > 2) { return false; }
            if (val[k] && typeof val[k] === 'object') { return false; }
        }
    }
    return true;
}

function treatAsArray (obj) {
  if (!Array.isArray(obj)) {
    return false;
  }
  if (runningLumen) {
    for (k in obj) {
      if (!digits(k)) {
        return false;
      }
    }
  }
  return true;
}

function digits (str) {
  return str.match(/^[0-9]+$/) !== null;
}

function trim (str) {
    return str ? str.replace(/^\s+|\s+$/g, "") : null;
}

// communication

function readAndRespond (buffer, data) {
    buffer.str = buffer.str + data;
    while (true) {
        var req = extractToplevelRequest(buffer);
        if (req == null) { return; }
        var res = response(req);
        if (res) {
            clientSend(res);
        }
    }
}

function extractToplevelRequest (buffer) {
    var str = buffer.str;
    var need = buffer.need;
    var curly = null;
    if (need != null && str.length >= need) {
        var req = null;
        try {
            req = JSON.parse(str.substr(0, need));
        } catch (e) {
            log("Failed to parse " + need + " chars from:");
            log(buffer);
        }
        buffer.str = str.substr(need+1); // strip terminating newline
        delete buffer.need;
        return req;
    } else if (curly = str.indexOf('{'), curly != null && curly > 0) {
        buffer.need = parseFloat(str.substr(0, curly));
        buffer.str = str.substr(curly);
        return extractToplevelRequest(buffer);
    };
};

function clientSend (res) {
    var json = JSON.stringify(res);
    // delimit JSON strings and length-encode them so we know exactly
    // what to pass to the JSON reader on the Emacs side
    if (C) {
        C.out.write('\0' + json.length + json + '\n');
        return true;
    }
}

function ensureEnd (str, c) {
    if (str[str.length - 1] !== c) {
        return str + c;
    }
    return str;
}

global.numenLogPrefix = null;
global.numenLogClientMessages = false;

function log (str) {
    // writes to intercepted stdout so can send to client and/or log file
    console.log(str);
}

var stdout_write = process.stdout.write;

function ourStdout () {
    var args = Array.prototype.slice.call(arguments, 0);
    stdout_write.apply(process.stdout, args);
    if (C && C.remote) {
        C.out.write(args[0]);
    }
    numenLogFile(args[0]);
}

process.stdout.write = ourStdout;

numenLogFile = function (str) {
    if (numenLogPrefix) {
        str = ensureEnd(str, '\n');
        if (str[0] !== '\0' || numenLogClientMessages) {
            fs.appendFileSync(numenLogPrefix + today(), timestamp() + ' ' + str);
        }
    }
}

function timestamp () { return (new Date()).toISOString(); }
function today () { return timestamp().split('T')[0]; }

function listen (in_, out, port) {
    if (!C) {
        var buffer = { 'str' : '' };
        in_.on('data', function (data) { readAndRespond(buffer, data); });
        C = { 'in_' : in_, 'out' : out };
        if (port) {
            var addr = out.remoteAddress;
            C.remote = addr;
            log('hello ' + addr);
            out.write('Ready on port ' + port + '.\n');
            in_.on('end', function () {
                C = null;
                log('goodbye ' + addr);
            });
        }
    } else {
        out.end('Another client has the REPL.\n');
    }
}

launchNumen = function (lang, port, logpath, loadpath) {
    runningLumen = ((lang || "").toLowerCase() == "lumen");
    if (logpath) {
        numenLogPrefix = ensureEnd(logpath, '/') + "numen-";
    }
    process.on('uncaughtException', sendException);
    if (!port) {
        listen(process.stdin, process.stdout);
    } else if (!S) {
        var pidfile = "/tmp/numenpid" + port;
        process.on('exit', function () {
            log('exit');
            try { fs.unlinkSync(pidfile); }
            catch (e) {}
        });
        process.on('SIGTERM', function () {
            log('sigterm');
            process.exit();
        });
        S = net.createServer(function (c) { listen(c, c, port); });
        S.listen(port, function () {
            fs.writeFileSync(pidfile, process.pid);
            log('Numen ' + process.pid + ' is listening on port ' + port);
            if (loadpath) {
                log('loading ' + loadpath + '...');
                numenLoad(loadpath, null, null, 88); // arbitrary exit code for load failure
            }
        });
    }
}
