# -*- coding: utf-8 -*-

from __future__ import division, unicode_literals

from . import functions
from . import environments
from . import MacroExpander
from . import symbols
from . import utils
from . import parseData
from . import ParseError
from .parseData import ParseNode
from .unicodeRegexes import cjkRegex

# This file contains the parser used to parse out a TeX expression from the
# input. Since TeX isn't context-free, standard parsers don't work particularly
# well.
#
# The strategy of this parser is as such:
#
# The main functions (the `.parse...` ones) take a position in the current
# parse string to parse tokens from. The lexer (found in Lexer.js, stored at
# self.lexer) also supports pulling out tokens at arbitrary places. When
# individual tokens are needed at a position, the lexer is called to pull out a
# token, which is then used.
#
# The parser has a property called "mode" indicating the mode that
# the parser is currently in. Currently it has to be one of "math" or
# "text", which denotes whether the current environment is a math-y
# one or a text-y one (e.g. inside \text). Currently, this serves to
# limit the functions which can be used in text mode.
#
# The main functions then return an object which contains the useful data that
# was parsed at its given point, and a new position at the end of the parsed
# data. The main functions can call each other and continue the parsing by
# using the returned position as a new starting point.
#
# There are also extra `.handle...` functions, which pull out some reused
# functionality into self-contained functions.
#
# The earlier functions return ParseNodes.
# The later functions (which are called deeper in the parse) sometimes return
# ParseFuncOrArgument, which contain a ParseNode as well as some data about
# whether the parsed object is a function which is missing some arguments, or a
# standalone object which can be used as an argument to another function.

# Main Parser class
class Parser(object):
    def __init__(input, settings):
        # Create a new macro expander (gullet) and (indirectly via that) also a
        # new lexer (mouth) for this parser (stomach, in the language of TeX)
        self.gullet = MacroExpander(input, settings.macros)
        # Store the settings for use in parsing
        self.settings = settings

        self.ParseNode = ParseNode

    # Checks a result to make sure it has the right type, and throws an
    # appropriate error otherwise.
    #
    # @param {boolean=} consume whether to consume the expected token,
    #                           defaults to True
    def expect(text, consume):
        if self.nextToken.text != text:
            utils.die("Expected '{0}', got '{1}'".format(text, self.nextToken.text))
        if consume:
            self.consume()

    # Considers the current look ahead token as consumed,
    # and fetches the one after that as the new look ahead.
    def consume():
        self.nextToken = self.gullet.get(self.mode == "math")

    def switchMode(newMode):
        self.gullet.unget(self.nextToken)
        self.mode = newMode
        self.consume()

    # Main parsing function, which parses an entire input.
    #
    # @return {?Array.<ParseNode>}
    def parse():
        # Try to parse the input
        self.mode = "math"
        self.consume()
        parse = self.parseInput()
        return parse

    # Parses an entire input tree.
    def parseInput():
        # Parse an expression
        expression = self.parseExpression(False)
        # If we succeeded, make sure there's an EOF at the end
        self.expect("EOF", False)
        return expression

    # Parses an "expression", which is a list of atoms.
    #
    # @param {boolean} breakOnInfix  Should the parsing stop when we hit infix
    #                  nodes? This happens when functions have higher precendence
    #                  than infix nodes in implicit parses.
    #
    # @param {?string} breakOnTokenText  The text of the token that the expression
    #                  should end with, or `null` if something else should end the
    #                  expression.
    #
    # @return {ParseNode}
    def parseExpression(breakOnInfix, breakOnTokenText):
        endOfExpression = ["}", "\\end", "\\right", "&", "\\\\", "\\cr"]
        body = []
        # Keep adding atoms to the body until we can't parse any more atoms (either
        # we reached the end, a }, or a \right)
        while True:
            lex = self.nextToken
            if lex.text in endOfExpression:
                break
            if breakOnTokenText and lex.text == breakOnTokenText:
                break
            if breakOnInfix and lex.text in functions and functions[lex.text].infix:
                break
            atom = self.parseAtom()
            if not atom:
                if not self.settings.throwOnError and lex.text.startswith("\\"):
                    errorNode = self.handleUnsupportedCmd()
                    body.append(errorNode)
                    continue
                break
            body.append(atom)
        return self.handleInfixNodes(body)

    # Rewrites infix operators such as \over with corresponding commands such
    # as \frac.
    #
    # There can only be one infix operator per group.  If there's more than one
    # then the expression is ambiguous.  This can be resolved by adding {}.
    #
    # @returns {Array}
    def handleInfixNodes(body):
        overIndex = None

        for i,node in enumerate(body):
            if node.type == "infix":
                if overIndex is not None:
                    raise Exception(
                        "only one infix operator per group",
                        node.value.token)
                overIndex = i
                funcName = node.value.replaceWith

        if overIndex is not None:
            numerBody = body[:overIndex]
            denomBody = body[overIndex:]

            if len(numerBody) == 1 and numerBody[0].type == "ordgroup":
                numerNode = numerBody[0]
            else:
                numerNode = ParseNode("ordgroup", numerBody, self.mode)

            if len(denomBody) == 1 and denomBody[0].type == "ordgroup":
                denomNode = denomBody[0]
            else:
                denomNode = ParseNode("ordgroup", denomBody, self.mode)

            value = self.callFunction(funcName, [numerNode, denomNode], None)
            return [ParseNode(value.type, value, self.mode)]
        else:
            return body

    # Handle a subscript or superscript with nice errors.
    def handleSupSubscript(name):
        symbolToken = self.nextToken
        symbol = symbolToken.text
        self.consume()
        group = self.parseGroup()

        if not group:
            if not self.settings.throwOnError and self.nextToken.text.startswith("\\"):
                return self.handleUnsupportedCmd()
            else
                raise Exception(
                    "Expected group after '" + symbol + "'",
                    symbolToken
                )
        elif group.isFunction:
            # ^ and _ have a greediness, so handle interactions with functions'
            # greediness
            funcGreediness = functions[group.result].greediness
            if funcGreediness > SUPSUB_GREEDINESS:
                return self.parseFunction(group)
            else:
                raise Exception(
                    "Got function '{0}' with no arguments as {1}".format(group.result, name),
                    symbolToken)
        else:
            return group.result

    # Converts the textual input of an unsupported command into a text node
    # contained within a color node whose color is determined by errorColor
    def handleUnsupportedCmd():
        text = self.nextToken.text
        textordArray = [ParseNode("textord", t, "text") for t in text]

        textNode = ParseNode(
            "text",
            {
                "body": textordArray,
                "type": "text",
            },
            self.mode)

        colorNode = ParseNode(
            "color",
            {
                "color": self.settings.errorColor,
                "value": [textNode],
                "type": "color",
            },
            self.mode)

        self.consume()
        return colorNode

    # Parses a group with optional super/subscripts.
    #
    # @return {?ParseNode}
    def parseAtom():
        # The body of an atom is an implicit group, so that things like
        # \left(x\right)^2 work correctly.
        base = self.parseImplicitGroup()

        # In text mode, we don't have superscripts or subscripts
        if self.mode == "text":
            return base

        # Note that base may be empty (i.e. null) at this point.

        while True:
            # Lex the first token
            lex = self.nextToken

            if lex.text in ["\\limits", "\\nolimits"]:
                # We got a limit control
                if not base or base.type != "op":
                    raise Exception(
                        "Limit controls must follow a math operator",
                        lex)
                else:
                    base.value.limits = lex.text == "\\limits"
                    base.value.alwaysHandleSupSub = True
                self.consume()
            elif lex.text == "^":
                # We got a superscript start
                if superscript:
                    raise Exception("Double superscript", lex)
                superscript = self.handleSupSubscript("superscript")
            elif lex.text == "_":
                # We got a subscript start
                if subscript:
                    raise Exception("Double subscript", lex)
                subscript = self.handleSupSubscript("subscript")
            elif lex.text == "'":
                # We got a prime
                prime = ParseNode("textord", "\\prime", self.mode)

                # Many primes can be grouped together, so we handle this here
                primes = [prime]
                self.consume()
                # Keep lexing tokens until we get something that's not a prime
                while self.nextToken.text == "'":
                    # For each one, add another prime to the list
                    primes.append(prime)
                    self.consume()
                # Put them into an ordgroup as the superscript
                superscript = ParseNode("ordgroup", primes, self.mode)
            else:
                # If it wasn't ^, _, or ', stop parsing super/subscripts
                break

        if superscript or subscript:
            # If we got either a superscript or subscript, create a supsub
            return ParseNode("supsub", {
                "base": base,
                "sup": superscript,
                "sub": subscript,
            }, self.mode)
        else:
            # Otherwise return the original body
            return base


    # Parses an implicit group, which is a group that starts at the end of a
    # specified, and ends right before a higher explicit group ends, or at EOL. It
    # is used for functions that appear to affect the current style, like \Large or
    # \textrm, where instead of keeping a style we just pretend that there is an
    # implicit grouping after it until the end of the group. E.g.
    #   small text {\Large large text} small text again
    # It is also used for \left and \right to get the correct grouping.
    #
    # @return {?ParseNode}
    def parseImplicitGroup():
        sizeFuncs = [
            "\\tiny", "\\scriptsize", "\\footnotesize", "\\small", "\\normalsize",
            "\\large", "\\Large", "\\LARGE", "\\huge", "\\Huge",
        ]
        styleFuncs = [
            "\\displaystyle", "\\textstyle", "\\scriptstyle", "\\scriptscriptstyle",
        ]
        start = self.parseSymbol()

        if not start:
            # If we didn't get anything we handle, fall back to parseFunction
            return self.parseFunction()

        func = start.result

        if func == "\\left":
            # If we see a left:
            # Parse the entire left function (including the delimiter)
            left = self.parseFunction(start)
            # Parse out the implicit body
            body = self.parseExpression(False)
            # Check the next token
            self.expect("\\right", False)
            right = self.parseFunction()
            return ParseNode("leftright", {
                "body": body,
                "left": left.value.value,
                "right": right.value.value,
            }, self.mode)
        elif func == "\\begin":
            # begin...end is similar to left...right
            begin = self.parseFunction(start)
            envName = begin.value.name
            env = environments.get(envName)
            if not env:
                raise Exception(
                    "No such environment: " + envName, begin.value.nameGroup)
            # Build the environment object. Arguments and other information will
            # be made available to the begin and end methods using properties.
            args = self.parseArguments("\\begin{" + envName + "}", env)
            context = {
                mode: self.mode,
                envName: envName,
                parser: this,
                positions: args.pop(),
            }
            result = env.handler(context, args)
            self.expect("\\end", False)
            endNameToken = self.nextToken
            end = self.parseFunction()
            if end.value.name != envName:
                raise Exception(
                    "Mismatch: \\begin{" + envName + "} matched " +
                    "by \\end{" + end.value.name + "}",
                    endNameToken)
            }
            result.position = end.position
            return result
        elif func in sizeFuncs:
            # If we see a sizing function, parse out the implict body
            body = self.parseExpression(False)
            return ParseNode("sizing", {
                # Figure out what size to use based on the list of functions above
                "size": "size" + (utils.indexOf(sizeFuncs, func) + 1),
                "value": body,
            }, self.mode)
        elif func in styleFuncs:
            # If we see a styling function, parse out the implict body
            body = self.parseExpression(True)
            return ParseNode("styling", {
                # Figure out what style to use by pulling out the style from
                # the function name
                "style": func.slice(1, func.length - 5),
                "value": body,
            }, self.mode)
        else:
            # Defer to parseFunction if it's not a function we handle
            return self.parseFunction(start)

    # Parses an entire function, including its base and all of its arguments.
    # The base might either have been parsed already, in which case
    # it is provided as an argument, or it's the next group in the input.
    #
    # @param {ParseFuncOrArgument=} baseGroup optional as described above
    # @return {?ParseNode}
    def parseFunction(baseGroup):
        if not baseGroup:
            baseGroup = self.parseGroup()

        if baseGroup:
            if baseGroup.isFunction:
                func = baseGroup.result
                funcData = functions[func]
                if self.mode == "text" and not funcData.allowedInText:
                    raise Exception(
                        "Can't use function '" + func + "' in text mode",
                        baseGroup.token)

                args = self.parseArguments(func, funcData)
                token = baseGroup.token
                result = self.callFunction(func, args, args.pop(), token)
                return ParseNode(result.type, result, self.mode)
            else:
                return baseGroup.result

    # Call a function handler with a suitable context and arguments.
    def callFunction(name, args, positions, token):
        context = {
            "funcName": name,
            "parser": this,
            "positions": positions,
            "token": token,
        }
        return functions[name].handler(context, args)

    # Parses the arguments of a function or environment
    #
    # @param {string} func  "\name" or "\begin{name}"
    # @param {{numArgs:number,numOptionalArgs:number|undefined}} funcData
    # @return the array of arguments, with the list of positions as last element
    def parseArguments(func, funcData):
        totalArgs = funcData.numArgs + funcData.numOptionalArgs
        if totalArgs == 0:
            return [[self.pos]]

        baseGreediness = funcData.greediness
        positions = [self.pos]
        args = []

        for i in range(totalArgs):
            nextToken = self.nextToken
            argtype = funcData.argTypes[i] if "argTypes" in funcData else None
            if i < funcData.numOptionalArgs:
                if argType:
                    arg = self.parseGroupOfType(argType, True)
                else:
                    arg = self.parseGroup(True)
                if not arg:
                    args.append(None)
                    positions.append(self.pos)
                    continue
            else:
                if argType:
                    arg = self.parseGroupOfType(argType)
                else:
                    arg = self.parseGroup()
                if not arg:
                    if not self.settings.throwOnError and self.nextToken.text.startswith("\\"):
                        arg = ParseFuncOrArgument(
                            self.handleUnsupportedCmd(self.nextToken.text),
                            False)
                    else:
                        raise Exception(
                            "Expected group after '" + func + "'", nextToken)
            if arg.isFunction:
                argGreediness = functions[arg.result].greediness
                if argGreediness > baseGreediness:
                    argNode = self.parseFunction(arg)
                else:
                    raise Exception(
                        "Got function '" + arg.result + "' as " +
                        "argument to '" + func + "'", nextToken)
            else:
                argNode = arg.result
            args.append(argNode)
            positions.append(self.pos)

        args.append(positions)

        return args


    # Parses a group when the mode is changing.
    #
    # @return {?ParseFuncOrArgument}
    def parseGroupOfType(innerMode, optional):
        outerMode = self.mode
        # Handle `original` argTypes
        if innerMode == "original":
            innerMode = outerMode
        if innerMode == "color":
            return self.parseColorGroup(optional)
        if innerMode == "size":
            return self.parseSizeGroup(optional)

        self.switchMode(innerMode)
        if innerMode == "text":
            # text mode is special because it should ignore the whitespace before
            # it
            while self.nextToken.text == " ":
                self.consume()
        # By the time we get here, innerMode is one of "text" or "math".
        # We switch the mode of the parser, recurse, then restore the old mode.
        res = self.parseGroup(optional)
        self.switchMode(outerMode)
        return res

    # Parses a group, essentially returning the string formed by the
    # brace-enclosed tokens plus some position information.
    #
    # @param {string} modeName  Used to describe the mode in error messages
    # @param {boolean=} optional  Whether the group is optional or required
    def parseStringGroup(modeName, optional):
        if optional and self.nextToken.text != "[":
            return None
        outerMode = self.mode
        self.mode = "text"
        self.expect("[" if optional else "{")
        str = ""
        firstToken = self.nextToken
        lastToken = firstToken
        while self.nextToken.text != ("]" if optional else "}"):
            if self.nextToken.text == "EOF":
                raise Exception(
                    "Unexpected end of input in " + modeName,
                    firstToken.range(self.nextToken, str))
            lastToken = self.nextToken
            str += lastToken.text
            self.consume()
        self.mode = outerMode
        self.expect("]" if optional else "}")
        return firstToken.range(lastToken, str)

    # Parses a color description.
    def parseColorGroup(optional):
        res = self.parseStringGroup("color", optional)
        if not res:
            return None
        match = re.match(r"^(#[A-Za-z0-9]+|[A-Za-z]+)$", res.text)
        if not match:
            raise Exception("Invalid color: '" + res.text + "'", res)
        return ParseFuncOrArgument(
            ParseNode("color", match.group(0), self.mode),
            False)

    # Parses a size specification, consisting of magnitude and unit.
    def parseSizeGroup(optional):
        res = self.parseStringGroup("size", optional)
        if not res:
            return None
        match = re.search(r"(-?)#(\d+(?:\.\d*)?|\.\d+)#([a-z]{2})", res.text)
        if not match:
            raise Exception("Invalid size: '" + res.text + "'", res)
        data = {
            "number": float(match.group(1) + match.group(2)), # sign + magnitude, cast to number
            "unit": match.group(3),
        }
        if data.unit not in ["em", "ex"]:
            raise Exception("Invalid unit: '" + data.unit + "'", res)
        return ParseFuncOrArgument(
            ParseNode("color", data, self.mode),
            False)

    # If the argument is False or absent, this parses an ordinary group,
    # which is either a single nucleus (like "x") or an expression
    # in braces (like "{x+y}").
    # If the argument is True, it parses either a bracket-delimited expression
    # (like "[x+y]") or returns null to indicate the absence of a
    # bracket-enclosed group.
    #
    # @param {boolean=} optional  Whether the group is optional or required
    # @return {?ParseFuncOrArgument}
    def parseGroup(optional):
        firstToken = self.nextToken
        # Try to parse an open brace
        if self.nextToken.text == ("[" if optional else "{"):
            # If we get a brace, parse an expression
            self.consume()
            expression = self.parseExpression(False, "]" if optional else None)
            lastToken = self.nextToken
            # Make sure we get a close brace
            self.expect("]" if optional else "}")
            if self.mode == "text":
                self.formLigatures(expression)
            return ParseFuncOrArgument(
                ParseNode("ordgroup", expression, self.mode,
                              firstToken, lastToken),
                False)
        else:
            # Otherwise, just return a nucleus, or nothing for an optional group
            return None if optional else self.parseSymbol()

    # Form ligature-like combinations of characters for text mode.
    # This includes inputs like "--", "---", "``" and "''".
    # The result will simply replace multiple textord nodes with a single
    # character in each value by a single textord node having multiple
    # characters in its value.  The representation is still ASCII source.
    #
    # @param {Array.<ParseNode>} group  the nodes of this group,
    #                                   list will be moified in place
    def formLigatures(group):
        newNodes = []
        maxIndex = len(group) - 1
        skip = 0
        for i,node in enumerate(group):
            # When you consume multiple nodes,
            # skip says how many were consumed,
            # and can be skipped over
            if skip:
                skip -= 1
                continue
            v = node.value
            if v == "-" and i+1 < maxIndex and group[i+1].value == "-":
                if i+2 < maxIndex and group[i+2].value == "-":
                    newNodes.append(ParseNode("textord", "---", "text", node, group[i+2]))
                    skip = 2
                    continue
                else:
                    newNodes.append(ParseNode("textord", "--", "text", node, group[i+1]))
                    skip = 1
                    continue
            if v in "'`" and i+1 < maxIndex and group[i+1].value == v:
                newNodes.append(ParseNode("textord", v*2, "text", node, group[i+1]))
                skip = 1
                continue
            newNodes.append(node)
        # Mutate the old array in-place
        group[:] = newNodes

# Parse a single symbol out of the string. Here, we handle both the functions
# we have defined, as well as the single character symbols
#
# @return {?ParseFuncOrArgument}
Parser.prototype.parseSymbol = function() {
    nucleus = self.nextToken

    if (functions[nucleus.text]) {
        self.consume()
        # If there exists a function with this name, we return the function and
        # say that it is a function.
        return ParseFuncOrArgument(
            nucleus.text,
            True, nucleus)
    } else if (symbols[self.mode][nucleus.text]) {
        self.consume()
        # Otherwise if this is a no-argument function, find the type it
        # corresponds to in the symbols map
        return ParseFuncOrArgument(
            ParseNode(symbols[self.mode][nucleus.text].group,
                          nucleus.text, self.mode, nucleus),
            False, nucleus)
    } else if (self.mode == "text" && cjkRegex.test(nucleus.text)) {
        self.consume()
        return ParseFuncOrArgument(
            ParseNode("textord", nucleus.text, self.mode, nucleus),
            False, nucleus)
    } else {
        return null
    }
}

# An initial function (without its arguments), or an argument to a function.
# The `result` argument should be a ParseNode.
class ParseFuncOrArgument(object):
    def __init__(result, isFunction, token):
        self.result = result
        # Is this a function (i.e. is it something defined in functions.js)?
        self.isFunction = isFunction
        self.token = token



# The greediness of a superscript or subscript
SUPSUB_GREEDINESS = 1
