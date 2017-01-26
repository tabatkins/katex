# This file converts a parse tree into a cooresponding MathML tree. The main
# entry point is the `buildMathML` function, which takes a parse tree from the
# parser.

from . import buildCommon
from . import fontMetrics
from . import mathMLTree
from . import ParseError
from . import symbols
from . import utils
from .buildCommon import makeSpan, fontMap


# Takes a symbol and converts it into a MathML text node after performing
# optional replacement from symbols.js.
def makeText(text, mode):
    text = symbols.replace(mode, text)
    return new mathMLTree.TextNode(text)

# Returns the math variant as a string or null if none is required.
def getVariant(group, options):
    font = options.font
    if not font:
        return

    mode = group.mode
    if font == "mathit":
        return "italic"

    value = group.value
    if value in ["\\imath", "\\jmath"]:
        return

    value = symbols.replace(mode, value)

    fontName = fontMap[font].fontName
    if fontMetrics.getCharacterMetrics(value, fontName):
        return fontMap[options.font].variant

# Functions for handling the different types of groups found in the parse
# tree. Each function should take a parse group and return a MathML node.
groupTypes = {}

def mathord(group, options):
    node = new mathMLTree.MathNode("mi", [makeText(group.value, group.mode)])

    variant = getVariant(group, options)
    if variant:
        node.setAttribute("mathvariant", variant)

    return node
groupTypes['mathord'] = mathord

def textord(group, options):
    text = makeText(group.value, group.mode)

    variant = getVariant(group, options) or "normal"

    if group.value.isdigit():
        # TODO(kevinb) merge adjacent <mn> nodes
        # do it as a post processing step
        node = new mathMLTree.MathNode("mn", [text])
        if options.font:
            node.setAttribute("mathvariant", variant)
    else:
        node = new mathMLTree.MathNode("mi", [text])
        node.setAttribute("mathvariant", variant)

    return node
groupTypes['textord'] = textord

def bin(group):
    node = new mathMLTree.MathNode("mo", [makeText(group.value, group.mode)])

    return node
groupTypes['bin'] = bin

def rel(group):
    node = new mathMLTree.MathNode("mo", [makeText(group.value, group.mode)])

    return node
groupTypes['rel'] = rel

def open(group):
    node = new mathMLTree.MathNode("mo", [makeText(group.value, group.mode)])

    return node
groupTypes['open'] = open

def close(group):
    node = new mathMLTree.MathNode("mo", [makeText(group.value, group.mode)])

    return node
groupTypes['close'] = close

def inner(group):
    node = new mathMLTree.MathNode("mo", [makeText(group.value, group.mode)])

    return node
groupTypes['inner'] = inner

def punct(group):
    node = new mathMLTree.MathNode("mo", [makeText(group.value, group.mode)])

    node.setAttribute("separator", "true")

    return node
groupTypes['punct'] = punct

def ordgroup(group, options):
    inner = buildExpression(group.value, options)

    node = new mathMLTree.MathNode("mrow", inner)

    return node
groupTypes['ordgroup'] = ordgroup

def text(group, options):
    inner = buildExpression(group.value.body, options)

    node = new mathMLTree.MathNode("mtext", inner)

    return node
groupTypes['text'] = text

def color(group, options):
    inner = buildExpression(group.value.value, options)

    node = new mathMLTree.MathNode("mstyle", inner)

    node.setAttribute("mathcolor", group.value.color)

    return node
groupTypes['color'] = color

def supsub(group, options):
    children = [buildGroup(group.value.base, options)]

    if group.value.sub:
        children.append(buildGroup(group.value.sub, options))

    if group.value.sup:
        children.append(buildGroup(group.value.sup, options))

    if not group.value.sub:
        nodeType = "msup"
    elif not group.value.sup:
        nodeType = "msub"
    else:
        nodeType = "msubsup"

    node = new mathMLTree.MathNode(nodeType, children)

    return node
groupTypes['supsub'] = supsub

def genfrac(group, options):
    node = new mathMLTree.MathNode(
        "mfrac",
        [buildGroup(group.value.numer, options),
         buildGroup(group.value.denom, options)])

    if not group.value.hasBarLine:
        node.setAttribute("linethickness", "0px")

    if group.value.leftDelim or group.value.rightDelim:
        withDelims = []

        if group.value.leftDelim:
            leftOp = new mathMLTree.MathNode(
                "mo", [new mathMLTree.TextNode(group.value.leftDelim)])

            leftOp.setAttribute("fence", "true")

            withDelims.append(leftOp)

        withDelims.append(node)

        if group.value.rightDelim:
            rightOp = new mathMLTree.MathNode(
                "mo", [new mathMLTree.TextNode(group.value.rightDelim)])

            rightOp.setAttribute("fence", "true")

            withDelims.append(rightOp)

        outerNode = new mathMLTree.MathNode("mrow", withDelims)

        return outerNode

    return node
groupTypes['genfrac'] = genfrac

def array(group, options):
    def cellFunc(cell):
        return new MathMLTree.MathNode("mtd", [buildGroup(cell, options)])
    def rowFunc(row):
        return new MathMLTree.MathNode("mtr", map(cellFunc, row))
    return new mathMLTree.MathNode("mtable", map(rowFunc(group.value.body)))
groupTypes['array'] = array

def sqrt(group, options):
    if group.value.index:
        node = new mathMLTree.MathNode(
            "mroot", [
                buildGroup(group.value.body, options),
                buildGroup(group.value.index, options),
            ])
    else:
        node = new mathMLTree.MathNode(
            "msqrt", [buildGroup(group.value.body, options)])

    return node
groupTypes['sqrt'] = sqrt

def leftright(group, options):
    inner = buildExpression(group.value.body, options)

    if group.value.left != ".":
        leftNode = new mathMLTree.MathNode(
            "mo", [makeText(group.value.left, group.mode)])

        leftNode.setAttribute("fence", "true")

        inner.insert(0, leftNode)

    if group.value.right != ".":
        rightNode = new mathMLTree.MathNode(
            "mo", [makeText(group.value.right, group.mode)])

        rightNode.setAttribute("fence", "true")

        inner.append(rightNode)

    outerNode = new mathMLTree.MathNode("mrow", inner)

    return outerNode
groupTypes['leftright'] = leftright

def accent(group, options):
    accentNode = new mathMLTree.MathNode("mo", [makeText(group.value.accent, group.mode)])

    node = new mathMLTree.MathNode("mover", [buildGroup(group.value.base, options), accentNode])

    node.setAttribute("accent", "true")

    return node
groupTypes['accent'] = accent

def spacing(group):
    if group.value in ["\\ ", "\\space", " ", "~"]:
        node = new mathMLTree.MathNode("mtext", [new mathMLTree.TextNode("\u00a0")])
    else:
        node = new mathMLTree.MathNode("mspace")

        node.setAttribute("width", buildCommon.spacingFunctions[group.value].size)

    return node
groupTypes['spacing'] = spacing

def op(group):
    # TODO(emily): handle big operators using the `largeop` attribute

    if group.value.symbol:
        # This is a symbol. Just add the symbol.
        node = new mathMLTree.MathNode("mo", [makeText(group.value.body, group.mode)])
    else:
        # This is a text operator. Add all of the characters from the
        # operator's name.
        # TODO(emily): Add a space in the middle of some of these
        # operators, like \limsup.
        node = new mathMLTree.MathNode("mi", [new mathMLTree.TextNode(group.value.body[1:])])

    return node
groupTypes['op'] = op

def katex(group):
    node = new mathMLTree.MathNode("mtext", [new mathMLTree.TextNode("KaTeX")])

    return node
groupTypes['katex'] = katex

def font(group, options):
    font = group.value.font
    return buildGroup(group.value.body, options.withFont(font))
groupTypes['font'] = font

def delimsizing(group):
    children = []

    if group.value.value != ".":
        children.append(makeText(group.value.value, group.mode))

    node = new mathMLTree.MathNode("mo", children)

    if group.value.delimType in ["open", "close"]:
        # Only some of the delimsizing functions act as fences, and they
        # return "open" or "close" delimTypes.
        node.setAttribute("fence", "true")
    else:
        # Explicitly disable fencing if it's not a fence, to override the
        # defaults.
        node.setAttribute("fence", "false")

    return node
groupTypes['delimsizing'] = delimsizing

def styling(group, options):
    inner = buildExpression(group.value.value, options)

    node = new mathMLTree.MathNode("mstyle", inner)

    styleAttributes = {
        "display": ["0", "true"],
        "text": ["0", "false"],
        "script": ["1", "false"],
        "scriptscript": ["2", "false"],
    }

    attr = styleAttributes[group.value.style]

    node.setAttribute("scriptlevel", attr[0])
    node.setAttribute("displaystyle", attr[1])

    return node
groupTypes['styling'] = styling

def sizing(group, options):
    inner = buildExpression(group.value.value, options)

    node = new mathMLTree.MathNode("mstyle", inner)

    # TODO(emily): This doesn't produce the correct size for nested size
    # changes, because we don't keep state of what style we're currently
    # in, so we can't reset the size to normal before changing it.  Now
    # that we're passing an options parameter we should be able to fix
    # this.
    node.setAttribute("mathsize", em(buildCommon.sizingMultiplier[group.value.size]))

    return node
groupTypes['sizing'] = sizing

def overline(group, options):
    operator = new mathMLTree.MathNode("mo", [new mathMLTree.TextNode("\u203e")])
    operator.setAttribute("stretchy", "true")

    node = new mathMLTree.MathNode("mover", [buildGroup(group.value.body, options), operator])
    node.setAttribute("accent", "true")

    return node
groupTypes['overline'] = overline

def underline(group, options):
    operator = new mathMLTree.MathNode("mo", [new mathMLTree.TextNode("\u203e")])
    operator.setAttribute("stretchy", "true")

    node = new mathMLTree.MathNode("munder", [buildGroup(group.value.body, options), operator])
    node.setAttribute("accentunder", "true")

    return node
groupTypes['underline'] = underline

def rule(group):
    # TODO(emily): Figure out if there's an actual way to draw black boxes
    # in MathML.
    node = new mathMLTree.MathNode("mrow")

    return node
groupTypes['rule'] = rule

def kern(group):
    # TODO(kevin): Figure out if there's a way to add space in MathML
    node = new mathMLTree.MathNode("mrow")

    return node
groupTypes['kern'] = kern

def llap(group, options):
    node = new mathMLTree.MathNode("mpadded", [buildGroup(group.value.body, options)])

    node.setAttribute("lspace", "-1width")
    node.setAttribute("width", "0px")

    return node
groupTypes['llap'] = llap

def rlap(group, options):
    node = new mathMLTree.MathNode("mpadded", [buildGroup(group.value.body, options)])

    node.setAttribute("width", "0px")

    return node
groupTypes['rlap'] = rlap

def phantom(group, options, prev):
    inner = buildExpression(group.value.value, options)
    return new mathMLTree.MathNode("mphantom", inner)
groupTypes['phantom'] = phantom

# Takes a list of nodes, builds them, and returns a list of the generated
# MathML nodes. A little simpler than the HTML version because we don't do any
# previous-node handling.
def buildExpression(expression, options):
    return [buildGroup(group, options) for group in expression]

# Takes a group from the parser and calls the appropriate groupTypes function
# on it to produce a MathML node.
def buildGroup(group, options):
    if not group:
        return new mathMLTree.MathNode("mrow")

    if group.type in groupTypes:
        # Call the groupTypes function
        return groupTypes[group.type](group, options)
    else:
        die("Got group of unknown type: '" + group.type + "'")

# Takes a full parse tree and settings and builds a MathML representation of
# it. In particular, we put the elements from building the parse tree into a
# <semantics> tag so we can also include that TeX source as an annotation.
#
# Note that we actually return a domTree element with a `<math>` inside it so
# we can do appropriate styling.
def buildMathML(tree, texExpression, options):
    expression = buildExpression(tree, options)

    # Wrap up the expression in an mrow so it is presented in the semantics
    # tag correctly.
    wrapper = new mathMLTree.MathNode("mrow", expression)

    # Build a TeX annotation of the source
    annotation = new mathMLTree.MathNode(
        "annotation", [new mathMLTree.TextNode(texExpression)])

    annotation.setAttribute("encoding", "application/x-tex")

    semantics = new mathMLTree.MathNode(
        "semantics", [wrapper, annotation])

    math = new mathMLTree.MathNode("math", [semantics])

    # You can't style <math> nodes, so we wrap the node in a span.
    return makeSpan(["katex-mathml"], [math])
