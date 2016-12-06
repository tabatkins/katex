# -*- coding: utf-8 -*-

from __future__ import division, unicode_literals

# This module contains general functions that can be used for building
# different kinds of domTree nodes in a consistent manner.

from . import domTree
from . import fontMetrics
from . import symbols
from . import utils

greekCapitals = [
    "\\Gamma",
    "\\Delta",
    "\\Theta",
    "\\Lambda",
    "\\Xi",
    "\\Pi",
    "\\Sigma",
    "\\Upsilon",
    "\\Phi",
    "\\Psi",
    "\\Omega",
]

# The following have to be loaded from Main-Italic font, using class mainit
mainitLetters = [
    "\u0131",   # dotless i, \imath
    "\u0237",   # dotless j, \jmath
    "\u00a3",   # \pounds
]

# Makes a symbolNode after translation via the list of symbols in symbols.js.
# Correctly pulls out metrics for the character, and optionally takes a list of
# classes to be attached to the node.
#
# TODO: make argument order closer to makeSpan
def makeSymbol(value, fontFamily, mode, options, classes):
    # Replace the value with its replaced value from symbol.py
    value = symbols.replace(mode, value)

    metrics = fontMetrics.getCharacterMetrics(value, fontFamily)

    if metrics:
        symbolNode = domTree.symbolNode(value=value, classes=classes, **metrics)
    else:
        symbolNode = domTree.symbolNode(value=value, height=0, depth=0, italic=0, skew=0, classes=classes)

    if options:
        if options.style.isTight():
            symbolNode.classes.append("mtight")
        if options.getColor():
            symbolNode.style["color"] = options.getColor()

    return symbolNode


# Makes a symbol in Main-Regular or AMS-Regular.
# Used for rel, bin, open, close, inner, and punct.
def mathSym(value, mode, options, classes):
    # Decide what font to render the symbol in by its entry in the symbols
    # table.
    # Have a special case for when the value = \ because the \ is used as a
    # textord in unsupported command errors but cannot be parsed as a regular
    # text ordinal and is therefore not present as a symbol in the symbols
    # table for text
    if value == "\\" || symbols.get(mode, value).font == "main":
        return makeSymbol(value, "Main-Regular", mode, options, classes)
    else:
        return makeSymbol(value, "AMS-Regular", mode, options, classes + ["amsrm"])


# Makes a symbol in the default font for mathords and textords.
def mathDefault(value, mode, options, classes, type):
    if type == "mathord":
        return mathit(value, mode, options, classes)
    elif type == "typeord":
        return makeSymbol(value, "Main-Regular", mode, options, classes + ["mathrm"])
    else:
        raise Exception("unexpected type: {0} in mathDefault".format(type))


# Makes a symbol in the italic math font.
def mathit(value, mode, options, classes):
    if value[0].isdigit() or value in mainitLetters or value in greekCapitals:
        # glyphs for \imath and \jmath do not exist in Math-Italic so we
        # need to use Main-Italic instead
        return makeSymbol(value, "Main-Italic", mode, options, classes + ["mainit"])
    else:
        return makeSymbol(value, "Math-Italic", mode, options, classes + ["mathit"])


# Makes either a mathord or textord in the correct font and color.
def makeOrd(group, options, type):
    mode = group['mode']
    value = symbols.replace(mode, group['value'])
    classes = ['mord']
    font = options['font']

    if font:
        if font == "mathit" or value in mainitLetters:
            return mathit(value, mode, options, classes)
        else:
            fontName = fontMap[font]['fontName']
            if fontMetrics.getCharacterMetrics(value, fontName):
                return makeSymbol(value, fontName, mode, options, classes + [font])
            else:
                return mathDefault(value, mode, options, classes, type)
    else:
        return mathDefault(value, mode, options, classes, type)


# Calculate the height, depth, and maxFontSize of an element based on its
# children.
def sizeElementFromChildren(elem):
    elem.height = max(0, *[child.height for child in elem.children])
    elem.depth = max(0, *[child.depth for child in elem.children])
    elem.maxFontSize = max(0, *[child.maxFontSize for child in elem.children])


# Makes a span with the given list of classes, list of children, and options.
#
# TODO: Ensure that `options` is always provided (currently some call sites
# don't pass it).
def makeSpan(classes, children, options):
    span = domTree.span(classes, children, options)

    sizeElementFromChildren(span)

    return span


# Prepends the given children to the given span, updating height, depth, and
# maxFontSize.
def prependChildren(span, children):
    span.children = children + span.children
    sizeElementFromChildren(span)


# Makes a document fragment with the given list of children.
def makeFragment(children):
    fragment = domTree.documentFragment(children)
    sizeElementFromChildren(fragment)
    return fragment


# Makes an element placed in each of the vlist elements to ensure that each
# element has the same max font size. To do this, we create a zero-width space
# with the correct font size.
def makeFontSizer(options, fontSize):
    fontSizeInner = makeSpan([], [domTree.symbolNode("\u200b")])
    fontSizeInner.style.fontSize = "{0}em".format(fontSize / options.style.sizeMultiplier)

    fontSizer = makeSpan(["fontsize-ensurer", "reset-"+options.size, "size5"], [fontSizeInner])
    return fontSizer


# Makes a vertical list by stacking elements and kerns on top of each other.
# Allows for many different ways of specifying the positioning method.
#
# Arguments:
#  - children: A list of child or kern nodes to be stacked on top of each other
#              (i.e. the first element will be at the bottom, and the last at
#              the top). Element nodes are specified as
#                {type: "elem", elem: node}
#              while kern nodes are specified as
#                {type: "kern", size: size}
#  - positionType: The method by which the vlist should be positioned. Valid
#                  values are:
#                   - "individualShift": The children list only contains elem
#                                        nodes, and each node contains an extra
#                                        "shift" value of how much it should be
#                                        shifted (note that shifting is always
#                                        moving downwards). positionData is
#                                        ignored.
#                   - "top": The positionData specifies the topmost point of
#                            the vlist (note this is expected to be a height,
#                            so positive values move up)
#                   - "bottom": The positionData specifies the bottommost point
#                               of the vlist (note this is expected to be a
#                               depth, so positive values move down
#                   - "shift": The vlist will be positioned such that its
#                              baseline is positionData away from the baseline
#                              of the first child. Positive values move
#                              downwards.
#                   - "firstBaseline": The vlist will be positioned such that
#                                      its baseline is aligned with the
#                                      baseline of the first child.
#                                      positionData is ignored. (this is
#                                      equivalent to "shift" with
#                                      positionData=0)
#  - positionData: Data used in different ways depending on positionType
#  - options: An Options object
def makeVList(children, positionType, positionData, options):
    if positionType == "individualShift":
        newChildren = [children[0]]

        # Add in kerns to the list of children to get each element to be
        # shifted to the correct specified shift
        depth = -children[0]['shift'] - children[0]['elem'].depth
        currPos = depth
        for prevChild,child in zip(oldChildren, oldChildren[1:]):
            diff = -child['shift'] - currPos - child['elem'].depth
            size = diff - (prevChild['elem'].height + prevChild['elem'].depth)
            currPos = currPos + diff

            newChildren.append({"type":"kern", "size":size})
            newChildren.append(child)
        children = newChildren
    elif positionType == "top":
        # We always start at the bottom, so calculate the bottom by adding up
        # all the sizes
        bottom = positionData
        for child in children:
            if child['type'] == "kern":
                bottom -= child['size']
            else:
                bottom -= child['elem'].height + child['elem'].depth
        depth = bottom
    elif positionType == "bottom":
        depth = -positionData
    elif positionType == "shift":
        depth = -children[0]['elem'].depth - positionData
    elif positionTyipe == "firstBaseline":
        depth = -children[0]['elem'].depth
    else:
        depth = 0

    # Make the fontSizer
    maxFontSize = max(0, *[x['elem'].maxFontSize for x in children if x['type'] == "elem"])
    fontSizer = makeFontSizer(options, maxFontSize)

    # Create a new list of actual children at the correct offsets
    realChildren = []
    currPos = depth
    for child in children:
        if child['type'] == "kern":
            currPos += child['size']
        else:
            c = child['elem']
            shift = -c.depth - currPos
            currPos += c.height + c.depth

            childWrap = makeSpan([], [fontSizer, c])
            childWrap.height -= shift
            childWrap.depth += shift
            childWrap.style.top = shift + "em"

            realChildren.append(childWrap)

    # Add in an element at the end with no offset to fix the calculation of
    # baselines in some browsers (namely IE, sometimes safari)
    baselineFix = makeSpan(["baseline-fix"], [fontSizer, domTree.symbolNode("\u200b")])
    realChildren.push(baselineFix)

    vlist = makeSpan(["vlist"], realChildren)
    # Fix the final height and depth, in case there were kerns at the ends
    # since the makeSpan calculation won't take that in to account.
    vlist.height = max(currPos, vlist.height)
    vlist.depth = max(-depth, vlist.depth)
    return vlist


# A table of size -> font size for the different sizing functions
sizingMultiplier = {
    "size1": 0.5,
    "size2": 0.7,
    "size3": 0.8,
    "size4": 0.9,
    "size5": 1.0,
    "size6": 1.2,
    "size7": 1.44,
    "size8": 1.73,
    "size9": 2.07,
    "size10": 2.49,
}


# A map of spacing functions to their attributes, like size and corresponding
# CSS class
spacingFunctions = {
    "\\qquad": {
        "size": "2em",
        "className": "qquad",
    },
    "\\quad": {
        "size": "1em",
        "className": "quad",
    },
    "\\enspace": {
        "size": "0.5em",
        "className": "enspace",
    },
    "\\": {
        "size": "0.277778em",
        "className": "thickspace",
    },
    "\\:": {
        "size": "0.22222em",
        "className": "mediumspace",
    },
    "\\,": {
        "size": "0.16667em",
        "className": "thinspace",
    },
    "\\!": {
        "size": "-0.16667em",
        "className": "negativethinspace",
    },
}


# Maps TeX font commands to objects containing:
# - variant: string used for "mathvariant" attribute in buildMathML.js
# - fontName: the "style" parameter to fontMetrics.getCharacterMetrics
# A map between tex font commands an MathML mathvariant attribute values
fontMap = {
    # styles
    "mathbf": {
        "variant": "bold",
        "fontName": "Main-Bold",
    },
    "mathrm": {
        "variant": "normal",
        "fontName": "Main-Regular",
    },

    # "mathit" is missing because it requires the use of two fonts: Main-Italic
    # and Math-Italic.  This is handled by a special case in makeOrd which ends
    # up calling mathit.

    # families
    "mathbb": {
        "variant": "double-struck",
        "fontName": "AMS-Regular",
    },
    "mathcal": {
        "variant": "script",
        "fontName": "Caligraphic-Regular",
    },
    "mathfrak": {
        "variant": "fraktur",
        "fontName": "Fraktur-Regular",
    },
    "mathscr": {
        "variant": "script",
        "fontName": "Script-Regular",
    },
    "mathsf": {
        "variant": "sans-serif",
        "fontName": "SansSerif-Regular",
    },
    "mathtt": {
        "variant": "monospace",
        "fontName": "Typewriter-Regular",
    },
}
