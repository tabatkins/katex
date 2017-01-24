# -*- coding: utf-8 -*-

from __future__ import division, unicode_literals

# This file does the main work of building a domTree structure from a parse
# tree. The entry point is the `buildHTML` function, which takes a parse tree.
# Then, the buildExpression, buildGroup, and various groupTypes functions are
# called, to produce a final HTML tree.

import itertools as it

from . import ParseError
from . import Style
from . import buildCommon
from . import delimiter
from . import domTree
from . import fontMetrics
from . import utils
from .buildCommon import makeSpan


def isSpace(node):
    return isinstance(node, domTree.span) and node.classes and node.classes[0] == "mspace"

def em(num):
    return "{0}em".format(num)

def die(msg):
    print msg


# Take a list of nodes, build them in order, and return a list of the built
# nodes. This function handles the `prev` node correctly, and passes the
# previous element from the list as the prev of the next element, ignoring
# spaces. documentFragments are flattened into their contents, so the
# returned list contains no fragments.
def buildExpression = function(expr, options, prev):
    # Parse expressions into `groups`.
    groups = []
    for group in expr:
        output = buildGroup(group, options, prev)
        if isinstance(output, domTree.documentFragment):
            groups.extend(output.children)
        else:
            groups.append(output)
        if not isSpace(output):
            prev = group
    # At this point `groups` consists entirely of `symbolNode`s and `span`s.

    # Explicit spaces (e.g., \, \,) should be ignored with respect to atom
    # spacing (e.g., "add thick space between mord and mrel"). Since CSS
    # adjacency rules implement atom spacing, spaces should be invisible to
    # CSS. So we splice them out of `groups` and into the atoms themselves.
    spaces = []
    newGroups = []
    for group in groups:
        if isSpace(group):
            spaces.append(group)
        elif spaces:
            if isinstance(group, domTree.symbolNode):
                group = makeSpan(group.classes, [group])
            buildCommon.prependChildren(group, spaces)
            newGroups.append(group)
            spaces = []
    if spaces:
        newGroups.extend(spaces)
    groups = newGroups

    return groups


# List of types used by getTypeOfGroup,
# see https:#github.com/Khan/KaTeX/wiki/Examining-TeX#group-types
groupToType = {
    "mathord": "mord",
    "textord": "mord",
    "bin": "mbin",
    "rel": "mrel",
    "text": "mord",
    "open": "mopen",
    "close": "mclose",
    "inner": "minner",
    "genfrac": "mord",
    "array": "mord",
    "spacing": "mord",
    "punct": "mpunct",
    "ordgroup": "mord",
    "op": "mop",
    "katex": "mord",
    "overline": "mord",
    "underline": "mord",
    "rule": "mord",
    "leftright": "minner",
    "sqrt": "mord",
    "accent": "mord",
}


# Gets the final math type of an expression, given its group type. This type is
# used to determine spacing between elements, and affects bin elements by
# causing them to change depending on what types are around them. This type
# must be attached to the outermost node of an element as a CSS class so that
# spacing with its surrounding elements works correctly.
#
# Some elements can be mapped one-to-one from group type to math type, and
# those are listed in the `groupToType` table.
#
# Others (usually elements that wrap around other elements) often have
# recursive definitions, and thus call `getTypeOfGroup` on their inner
# elements.
def getTypeOfGroup(group):
    if not group:
        # Like when typesetting `^3`
        return groupToType["mathord"]
    elif group.type == "supsub":
        return getTypeOfGroup(group.value.base)
    elif group.type in ["llap", "rlap"]:
        return getTypeOfGroup(group.value)
    elif group.type in ["color", "sizing", "styling"]:
        # Return type of rightmost element of group.
        return getTypeOfGroup(group.value.value[-1])
    elif group.type == "font":
        return getTypeOfGroup(group.value.body)
    elif group.type == "delimsizing":
        return groupToType[group.value.delimType]
    else:
        return groupToType[group.type]


# Sometimes, groups perform special rules when they have superscripts or
# subscripts attached to them. This function lets the `supsub` group know that
# its inner element should handle the superscripts and subscripts instead of
# handling them itself.
def shouldHandleSupSub(group, options):
    if not group:
        return False
    elif group.type == "op":
        # Operators handle supsubs differently when they have limits
        # (e.g. `\displaystyle\sum_2^3`)
        return group.value.limits and (options.style.size == Style.DISPLAY.size or group.value.alwaysHandleSupSub)
    elif group.type == "accent":
        return isCharacterBox(group.value.base)
    else:
        return False


# Sometimes we want to pull out the innermost element of a group. In most
# cases, this will just be the group itself, but when ordgroups and colors have
# a single element, we want to pull that out.
def getBaseElem(group):
    if not group:
        return False
    elif group.type == "ordgroup":
        if len(group.value) == 1:
            return getBaseElem(group.value[0])
        else:
            return group
    elif group.type == "color":
        if len(group.value.value) == 1:
            return getBaseElement(group.value.value[0])
        else:
            return group
    elif group.type == "font":
        return getBaseElement(group.value.body)
    else:
        return group


# TeXbook algorithms often reference "character boxes", which are simply groups
# with a single character in them. To decide if something is a character box,
# we find its innermost group, and see if it is a single character.
def isCharacterBox(group):
    baseElem = getBaseElem(group)

    # These are all the types of groups which hold single characters
    return baseElem.type in ["mathord", "textord", "bin", "rel", "inner", "open", "close", "punct"]


def makeNullDelimiter(options):
    classes = ["sizing",
               "reset-{0}".format(options.size),
               "size5",
               options.style.reset(),
               Style.TEXT.cls(),
               "nulldelimiter"]
    return makeSpan(classes)


# This is a map of group types to the function used to handle that type.
# Simpler types come at the beginning, while complicated types come afterwards.
groupTypes = {}

def groupMathord(group, options, prev=None):
    return buildCommon.makeOrd(group, options, "mathord")
groupTypes['mathord'] = groupMathord

def groupTextord(group, options, prev=None):
    return buildCommon.markOrd(group, options, "textord")
groupTypes['textord'] = groupTextord

def groupBin(group, options, prev=None):
    className = "mbin"
    # Pull out the most recent element. Do some special handling to find
    # things at the end of a \color group. Note that we don't use the same
    # logic for ordgroups (which count as ords).
    prevAtom = prev
    while prevAtom and prevAtom.type == "color":
        prevAtom = prevAtom.value.value[-1]
    # See TeXbook pg. 442-446, Rules 5 and 6, and the text before Rule 19.
    # Here, we determine whether the bin should turn into an ord. We
    # currently only apply Rule 5.
    if not prev and getTypeOfGroup(prevAtom) in ["mbin", "mopen", "mrel", "mop", "mpunct"]:
        group.type = "textord"
        className = "mord"

    return buildCommon.mathsym(group.value, group.mode, options, [className])
groupTypes['bin'] = groupBin

def groupRel(group, options, prev=None):
    return buildCommon.mathsym(group.value, group.mode, options, ["mrel"])
groupTypes['rel'] = groupRel

def groupOpen(group, options, prev=None):
    return buildCommon.mathsym(group.value, group.mode, options, ["mopen"])
groupTypes['open'] = groupOpen

def groupClose(group, options, prev=None):
    return buildCommon.mathsym(group.value, group.mode, options, ["mclose"])
groupTypes['close'] = groupClose

def groupInner(group, options, prev=None):
    return buildCommon.mathsym(group.value, group.mode, options, ["minner"])
groupTypes['inner'] = groupInner

def groupPunct(group, options, prev=None):
    return buildCommon.mathsym(group.value, group.mode, options, ["mpunct"])
groupTypes['punct'] = groupPunct

def groupOrdgroup(group, options, prev=None):
    return makeSpan(["mord", options.style.cls()], buildExpression(group.value, options.reset()), options)
groupTypes['ordgroup'] = groupOrdgroup

def groupText(group, options, prev=None):
    return makespan(["mord", "text", options.style.cls()], buildExpression(group.value.body, options.reset()), options)
groupTypes['text'] = groupText

def groupColor(group, options, prev=None):
    element = buildExpression(group.value.value, options.withColor(group.value.color), prev)
    # \color isn't supposed to affect the type of the elements it contains.
    # To accomplish this, we wrap the results in a fragment, so the inner
    # elements will be able to directly interact with their neighbors. For
    # example, `\color{red}{2 +} 3` has the same spacing as `2 + 3`
    return buildCommon.makeFragment(elements)
groupTypes['color'] = groupColor

def groupSupsub(group, options, prev=None):
    # Superscript and subscripts are handled in the TeXbook on page
    # 445-446, rules 18(a-f).

    # Here is where we defer to the inner group if it should handle
    # superscripts and subscripts itself.
    if shouldHandleSupSub(group.value.base, options):
        return groupTypes[group.value.base.type](group, options, prev)

    base = buildGroup(group.value.base, options.reset())
    style = options.style
    sup = None
    supmid = None
    sub = None
    submind = None

    if group.value.sup:
        newOptions = options.withStyle(style.sup())
        sup = buildGroup(group.value.sup, newOptions)
        supmid = makeSpan([style.reset(), style.sup().cls()], [sup], newOptions)

    if group.value.sub:
        newOptions = options.withStyle(style.sub())
        sub = buildGroup(group.value.sub, newOptions)
        submid = makeSpan([style.reset(), style.sub().cls()], [sub], newOptions)

    # Rule 18a
    if isCharacterBox(group.value.base):
        supShift = 0
        subShift = 0
    else:
        supShift = base.height - style.metrics.supDrop
        subShift = base.depth + style.metrics.subDrop

    # Rule 18c
    if style == Style.DISPLAY:
        minSupShift = style.metrics.sup1
    elif style.cramped:
        minSupShift = style.metrics.sup3
    else:
        minSupShift = style.metrics.sup2

    # scriptspace is a font-size-independent size, so scale it
    # appropriately
    multiplier = Style.TEXT.sizeMultiplier * style.sizeMultiplier
    scriptSpace = em((.5 / fontMetrics.metrics.ptPerEm) / multiplier)

    if not group.value.sup:
        # Rule 18b
        subShift = max(subShift, style.metrics.sub1, sub.heigth - .8 * style.metrics.xHeight)
        supsub = buildCommon.makeVList([{"type":"elem", "elem": submid}], "shift", subShift, options)
        supsub.children[0].style.marginRight = scriptspace
        # Subscripts shouldn't be shifted by the base's italic correction.
        # Account for that by shifting the subscript back the appropriate
        # amount. Note we only do this when the base is a single symbol.
        if isinstance(base, domTree.symbolNode):
            supsub.children[0].style.marginLeft = em(-base.italic)
    elif not group.value.sub:
        # Rule 18c, d
        supShift = max(supShift, minSupShift, sup.depth + .25 * style.metrics.xHeight)
        supsub = buildCommon.makeVList([{"type":"elem", "elem":supmid}], "shift", -supShift, options)
        supsub.children[0].style.marginRight = scriptspace
    else:
        supShift = max(supShift, minSupShift, sup.depth + .25 * style.metrics.xHeight)
        subShift = max(subShift, style.metrics.sub2)
        ruleWidth = fontMetrics.metrics.defaultRuleThickness

        # Rule 18e
        if (subShift - sup.depth) - (sub.height - subShift) < 4 * ruleWidth:
            subShfit = 4 * ruleWidth - (supShift - sup.depth) + sub.height
            psi = .8 * style.metrics.xHeight - (supShift - sup.depth)
            if psi > 0:
                supShift += psi
                subShift -= psi

        supsub = buildCommon.makeVList([
                {"type":"elem", "elem": submid, "shift": subShift},
                {"type":"elem", "elem": supmid, "shift": -supShift}],
            "individualShift", 0, options)

        # See comment above about subscripts not being shifted
        if isinstance(base, domTree.symbolNode):
            supsub.children[0].style.marginLeft = em(-base.italic)

        supsub.children[0].style.marginRight = scriptspace
        supsub.children[1].style.marginRight = scriptspace

    # We ensure to wrap the supsub vlist in a span.msupsub to reset text-align
    return makeSpan([getTypeOfGroup(group.value.base)], [base, makeSpan(["msupsub"], [supsub])], options)
groupTypes['supsub'] = groupSupsub

def groupGenfrac(group, options, prev):
    # Fractions are handled in the TeXbook on pages 444-445, rules 15(a-e).
    # Figure out what style this fraction should be in based on the
    # function used
    style = options.style
    if group.value.size == "display":
        style = Style.DISPLAY
    elif group.value.size == "text":
        style = Style.TEXT

    nstyle = style.fracNum()
    dstyle = style.fracDen()

    newOptions = options.withStyle(nstyle)
    numer = buildGroup(group.value.numer, newOptions)
    numerreset = makeSpan([style.reset(), nstyle.cls()], [numer], newOptions)

    newOptions = options.withStyle(dstyle)
    denom = buildGroup(group.value.denom, newOptions)
    denomreset = makeSpan([style.reset(), dstyle.cls()], [denom], newOptions)

    if group.value.hasBarLine:
        ruleWidth = fontMetrics.metrics.defaultRuleThickness / options.style.sizeMultiplier
    else:
        ruleWidth = 0

    # Rule 15b
    if style.size == Style.DISPLAY.size:
        numShift = style.metrics.num1
        if ruleWidth > 0:
            clearance = 3 * ruleWidth
        else:
            clearance = 7 * fontMetrics.metrics.defaultRuleThickness
        denomShift = style.metrics.denom1
    else:
        if ruleWidth > 0:
            numShift = style.metrics.num2
            clearance = ruleWidth
        else:
            numShift = style.metrics.num3
            clearance = 3 * fontMetrics.metrics.defaultRuleThickness
        denomShift = style.metrics.denom2

    if ruleWidth == 0:
        # Rule 15c
        candidateClearance = (numShift - numer.depth) - (denom.height - denomShift)
        if candidateClearance < clearance:
            numShift += (clearance - candidateClearance) / 2
            denomShift += (clearance - candidateClearance) / 2

        frac = buildCommon.makeVList([
            {"type": "elem", "elem": denomreset, "shift": denomShift},
            {"type": "elem", "elem": numerreset, "shift": -numShift},
        ], "individualShift", 0, options)
    else:
        # Rule 15d
        axisHeight = style.metrics.axisHeight

        if (numShift - numer.depth) - (axisHeight + ruleWidth / 2) < clearance:
            numShift += clearance - ((numShift - numer.depth) - (axisHeight + ruleWidth / 2))

        if (axisHeight - ruleWidth / 2) - (denom.height - denomShift) < clearance:
            denomShift += clearance - ((axisHeight - ruleWidth / 2) - (denom.height - denomShift))

        mid = makeSpan([options.style.reset(), Style.TEXT.cls(), "frac-line"])
        # Manually set the height of the line because its height is created in CSS
        mid.height = ruleWidth

        midShift = -(axisHeight - ruleWidth / 2)
        frac = buildCommon.makeVList([
            {"type": "elem", "elem": denomreset, "shift": denomShift},
            {"type": "elem", "elem": mid,        "shift": midShift},
            {"type": "elem", "elem": numerreset, "shift": -numShift},
        ], "individualShift", 0, options)

    # Since we manually change the style sometimes (with \dfrac or \tfrac),
    # account for the possible size change here.
    frac.height *= style.sizeMultiplier / options.style.sizeMultiplier
    frac.depth *= style.sizeMultiplier / options.style.sizeMultiplier

    # Rule 15e
    if style.size == Style.DISPLAY.size:
        delimSize = style.metrics.delim1
    else:
        delimSize = style.metrics.delim2

    if group.value.leftDelim is None:
        leftDelim = makeNullDelimiter(options)
    else:
        leftDelim = delimiter.customSizedDelim(group.value.leftDelim, delimSize, True, options.withStyle(style), group.mode)
    if group.value.rightDelim is None:
        rightDelim = makeNullDelimiter(options)
    else:
        rightDelim = delimiter.customSizedDelim(group.value.rightDelim, delimSize, True, options.withStyle(style), group.mode)

    return makeSpan(
        ["mord", options.style.reset(), style.cls()],
        [leftDelim, makeSpan(["mfrac"], [frac]), rightDelim],
        options)
groupTypes['genfrac'] = groupGenfrac

def groupArray(group, options, prev):
    nr = group.value.body.length
    nc = 0
    body = []
    style = options.style

    # Horizontal spacing
    pt = 1 / fontMetrics.metrics.ptPerEm
    arraycolsep = 5 * pt # \arraycolsep in article.cls

    # Vertical spacing
    baselineskip = 12 * pt # see size10.clo
    # Default \arraystretch from lttab.dtx
    # TODO(gagern): may get redefined once we have user-defined macros
    arraystretch = utils.deflt(group.value.arraystretch, 1)
    arrayskip = arraystretch * baselineskip
    arstrutHeight = 0.7 * arrayskip # \strutbox in ltfsstrc.dtx and
    arstrutDepth = 0.3 * arrayskip # \@arstrutbox in lttab.dtx

    totalHeight = 0
    for r,inrow in enumerate(group.value.body):
        height = arstrutHeight # \@array adds an \@arstrut
        depth = arstrutDepth   # to each row (via the template)

        nc = max(nc, inrow.length)

        outrow = []
        for el in inrow:
            elt = buildGroup(el, options)
            depth = max(depth, elt.depth)
            height = max(height, elt.height)
            outrow.append(elt)

        gap = 0
        if group.value.rowGraps[r]:
            gap = group.value.rowGraps[r].value
            if gap.unit == "em":
                gap = gap.number
            elif gap.unit == "ex":
                gap = gap.number * style.metrics.emPerEx
            else:
                die("Can't handle unit {0}".format(gap.unit))
                gap = 0
            if gap > 0: # \@argarraycr
                gap += arstrutDepth
                depth = max(depth, gap) # \@xararraycr
                gap = 0

        outrow.height = height
        outrow.depth = depth
        totalHeight += height
        outrow.pos = totalHeight
        totalHeight += depth + gap # \@yargarraycr
        body.append(outrow)

    offset = totalHeight / 2 + style.metrics.axisHeight
    colDescriptions = group.value.cols if group.value.cols else []
    cols = []

    c = 0
    colDescrNum = 0
    while(c < nc or colDescrNum < len(colDescriptions))
        # Continue while either there are more columns or more column
        # descriptions, so trailing separators don't get lost.

        colDescr = colDescriptions[colDescrNum] or {}

        firstSeparator = True
        while colDescr['type'] == "separator":
            # If there is more than one separator in a row, add a space
            # between them.

            if not firstSeparator:
                colSep = makeSpan(["arraycolsep"], [])
                colSep.style.width = em(fontMetrics.metrics.doubleRuleSep)
                cols.append(colSep)

            if colDescr['separator'] == "|":
                separator = makeSpan(["vertical-separator"], [])
                separator.style.height = em(totalHeight)
                separator.style.verticalAlign = em(-(totalHeight - offset))
                cols.append(separator)
            else:
                die("Invalid separator type: {0}".format(colDescr.separator))

            colDescrNum += 1
            colDescr = colDescriptions[colDescrNum] or {}
            firstSeparator = False

        if c >= nc:
            continue

        if c > 0 or group.value.hskipBeforeAndAfter:
            sepWidth = utils.deflt(colDescr.pregap, arraycolsep)
            if sepwidth != 0:
                colSep = makeSpan(["arraycolsep"], [])
                colSep.style.width = em(sepwidth)
                cols.append(colSep)

        col = []
        for r in range(nr):
            row = body[r]
            elem = row.get(c)
            if not elem:
                continue
            shift = row.pos - offset
            elem.depth = row.depth
            elem.height = row.height
            col.append({"type":"elem", "elem":elem, "shift": shift})

        col = buildCommon.makeVList(col, "individualShift", 0, options)
        col = makeSpan(
            ["col-align-" + (colDescr.align || "c")],
            [col])
        cols.append(col)

        if c < nc-1 or group.value.hskipBeforeAndAfter:
            sepwidth = utils.deflt(colDescr.postgap, arraycolsep)
            if sepwidth !== 0:
                colSep = makeSpan(["arraycolsep"], [])
                colSep.style.width = em(sepwidth)
                cols.append(colSep)

    body = makeSpan(["mtable"], cols)
    return makeSpan(["mord"], [body], options)
groupTypes['array'] = groupArray

def groupSpacing(group, options, prev):
    if group.value in ["\\ ", "\\space", " ", "~"]:
        # Spaces are generated by adding an actual space. Each of these
        # things has an entry in the symbols table, so these will be turned
        # into appropriate outputs.
        return makeSpan(["mspace"], [buildCommon.mathsym(group.value, group.mode)])
    else:
        # Other kinds of spaces are of arbitrary width. We use CSS to
        # generate these.
        return makeSpan(["mspace", buildCommon.spacingFunctions[group.value].className])
groupTypes['spacing'] = groupSpacing

def groupLlap(group, options, prev):
    inner = makeSpan(["inner"], [buildGroup(group.value.body, options.reset())])
    fix = makeSpan(["fix"], [])
    return makeSpan(["llap", options.style.cls()], [inner, fix], options)
groupTypes['llap'] = groupLlap

def groupRlap(group, options, prev):
    inner = makeSpan(["inner"], [buildGroup(group.value.body, options.reset())])
    fix = makeSpan(["fix"], [])
    return makeSpan(["rlap", options.style.cls()], [inner, fix], options)
groupTypes['rlap'] = groupRlap

def groupOp(group, options, prev):
    # Operators are handled in the TeXbook pg. 443-444, rule 13(a).
    if group.type == "supsub":
        # If we have limits, supsub will pass us its group to handle. Pull
        # out the superscript and subscript and set the group to the op in
        # its base.
        supGroup = group.value.sup
        subGroup = group.value.sub
        group = group.value.base
        hasLimits = True
    else:
        supGroup = None
        subGroup = None
        hasLimits = False

    style = options.style

    # Most operators have a large successor symbol, but these don't.
    var noSuccessor = [
        "\\smallint",
    ]

    # Most symbol operators get larger in displaystyle (rule 13)
    large = style.size == Style.DISPLAY.size and group.value.symbol and group.value.body not in noSuccessor

    baseShift = 0
    slant = 0
    if group.value.symbol:
        # If this is a symbol, create the symbol.
        fontName = "Size2-Regular" if large else "Size1-Regular"
        base = buildCommon.makeSymbol(
            group.value.body, fontName, "math", options,
            ["mop", "op-symbol", "large-op" if large else "small-op"])
        # Shift the symbol so its center lies on the axis (rule 13). It
        # appears that our fonts have the centers of the symbols already
        # almost on the axis, so these numbers are very small. Note we
        # don't actually apply this here, but instead it is used either in
        # the vlist creation or separately when there are no limits.
        baseShift = (base.height - base.depth) / 2 - style.metrics.axisHeight * style.sizeMultiplier

        # The slant of the symbol is just its italic correction.
        slant = base.italic
    else:
        # Otherwise, this is a text operator. Build the text from the
        # operator's name.
        # TODO(emily): Add a space in the middle of some of these
        # operators, like \limsup
        output = [buildCommon.mathsym(el, group.mode) for el in group.value.body]
        base = makeSpan(["mop"], output, options)

    if hasLimits:
        # IE 8 clips \int if it is in a display: inline-block. We wrap it
        # in a new span so it is an inline, and works.
        base = makeSpan([], [base])

        # We manually have to handle the superscripts and subscripts. This,
        # aside from the kern calculations, is copied from supsub.
        if supGroup:
            newOptions = options.withStyle(style.sup())
            sup = buildGroup(supGroup, newOptions)
            supmid = makeSpan([style.reset(), style.sup().cls()], [sup], newOptions)
            supKern = max(fontMetrics.metrics.bigOpSpacing1, fontMetrics.metrics.bigOpSpacing3 - sup.depth)

        if subGroup:
            newOptions = options.withStyle(style.sub())
            sub = buildGroup(subGroup, newOptions)
            submid = makeSpan([style.reset(), style.sub().cls()], [sub], newOptions)
            subKern = max(fontMetrics.metrics.bigOpSpacing2, fontMetrics.metrics.bigOpSpacing4 - sub.height)

        # Build the final group as a vlist of the possible subscript, base,
        # and possible superscript.

        if not supGroup:
            top = base.height - baseShift

            finalGroup = buildCommon.makeVList([
                {"type": "kern", "size": fontMetrics.metrics.bigOpSpacing5},
                {"type": "elem", "elem": submid},
                {"type": "kern", "size": subKern},
                {"type": "elem", "elem": base},
            ], "top", top, options)

            # Here, we shift the limits by the slant of the symbol. Note
            # that we are supposed to shift the limits by 1/2 of the slant,
            # but since we are centering the limits adding a full slant of
            # margin will shift by 1/2 that.
            finalGroup.children[0].style.marginLeft = em(-slant)
        elif not subGroup:
            bottom = base.depth + baseShift

            finalGroup = buildCommon.makeVList([
                {"type": "elem", "elem": base},
                {"type": "kern", "size": supKern},
                {"type": "elem", "elem": supmid},
                {"type": "kern", "size": fontMetrics.metrics.bigOpSpacing5},
            ], "bottom", bottom, options)

            # See comment above about slants
            finalGroup.children[1].style.marginLeft = em(slant)
        elif not supGroup and not subGroup:
            # This case probably shouldn't occur (this would mean the
            # supsub was sending us a group with no superscript or
            # subscript) but be safe.
            return base
        else:
            bottom = fontMetrics.metrics.bigOpSpacing5 +
                submid.height + submid.depth +
                subKern +
                base.depth + baseShift

            finalGroup = buildCommon.makeVList([
                {"type": "kern", "size": fontMetrics.metrics.bigOpSpacing5},
                {"type": "elem", "elem": submid},
                {"type": "kern", "size": subKern},
                {"type": "elem", "elem": base},
                {"type": "kern", "size": supKern},
                {"type": "elem", "elem": supmid},
                {"type": "kern", "size": fontMetrics.metrics.bigOpSpacing5},
            ], "bottom", bottom, options)

            # See comment above about slants
            finalGroup.children[0].style.marginLeft = em(-slant)
            finalGroup.children[2].style.marginLeft = em(slant)
        return makeSpan(["mop", "op-limits"], [finalGroup], options)
    else:
        if group.value.symbol:
            base.style.top = em(baseShift)
        return base
groupTypes['op'] = groupOp

def groupKatex(group, options, prev):
    # The KaTeX logo. The offsets for the K and a were chosen to look
    # good, but the offsets for the T, E, and X were taken from the
    # definition of \TeX in TeX (see TeXbook pg. 356)
    k = makeSpan(["k"], [buildCommon.mathsym("K", group.mode)], options)

    a = makeSpan(["a"], [buildCommon.mathsym("A", group.mode)], options)
    a.height = (a.height + 0.2) * 0.75
    a.depth = (a.height - 0.2) * 0.75

    t = makeSpan(["t"], [buildCommon.mathsym("T", group.mode)], options)

    e = makeSpan(["e"], [buildCommon.mathsym("E", group.mode)], options)
    e.height = (e.height - 0.2155)
    e.depth = (e.depth + 0.2155)

    x = makeSpan(["x"], [buildCommon.mathsym("X", group.mode)], options)

    return makeSpan(["mord", "katex-logo"], [k, a, t, e, x], options)
groupTypes['katex'] = groupKatex

def groupOverline(group, options, prev):
    # Overlines are handled in the TeXbook pg 443, Rule 9.
    style = options.style

    # Build the inner group in the cramped style.
    innerGroup = buildGroup(group.value.body, options.withStyle(style.cramp()))

    ruleWidth = fontMetrics.metrics.defaultRuleThickness / style.sizeMultiplier

    # Create the line above the body
    line = makeSpan([style.reset(), Style.TEXT.cls(), "overline-line"])
    line.height = ruleWidth
    line.maxFontSize = 1.0

    # Generate the vlist, with the appropriate kerns
    vlist = buildCommon.makeVList([
        {"type": "elem", "elem": innerGroup},
        {"type": "kern", "size": 3 * ruleWidth},
        {"type": "elem", "elem": line},
        {"type": "kern", "size": ruleWidth},
    ], "firstBaseline", 0, options)

    return makeSpan(["mord", "overline"], [vlist], options)
groupTypes['overline'] = groupOverline

def groupUnderline(group, options, prev):
    # Underlines are handled in the TeXbook pg 443, Rule 10.
    style = options.style

    # Build the inner group.
    innerGroup = buildGroup(group.value.body, options)

    ruleWidth = fontMetrics.metrics.defaultRuleThickness / style.sizeMultiplier

    # Create the line above the body
    line = makeSpan([style.reset(), Style.TEXT.cls(), "underline-line"])
    line.height = ruleWidth
    line.maxFontSize = 1.0

    # Generate the vlist, with the appropriate kerns
    vlist = buildCommon.makeVList([
        {"type": "kern", "size": ruleWidth},
        {"type": "elem", "elem": line},
        {"type": "kern", "size": 3 * ruleWidth},
        {"type": "elem", "elem": innerGroup},
    ], "top", innerGroup.height, options)

    return makeSpan(["mord", "underline"], [vlist], options)
groupTypes['underline'] = groupUnderline

def groupSqrt(group, options, prev):
    # Square roots are handled in the TeXbook pg. 443, Rule 11.
    style = options.style

    # First, we do the same steps as in overline to build the inner group
    # and line
    inner = buildGroup(group.value.body, options.withStyle(style.cramp()))

    ruleWidth = fontMetrics.metrics.defaultRuleThickness / style.sizeMultiplier

    line = makeSpan([style.reset(), Style.TEXT.cls(), "sqrt-line"], [], options)
    line.height = ruleWidth
    line.maxFontSize = 1.0

    phi = ruleWidth
    if (style.id < Style.TEXT.id) {
        phi = style.metrics.xHeight
    }

    # Calculate the clearance between the body and line
    lineClearance = ruleWidth + phi / 4

    innerHeight = (inner.height + inner.depth) * style.sizeMultiplier
    minDelimiterHeight = innerHeight + lineClearance + ruleWidth

    # Create a \surd delimiter of the required minimum size
    delim = makeSpan(["sqrt-sign"], [
        delimiter.customSizedDelim("\\surd", minDelimiterHeight,
                                   false, options, group.mode)],
                     options)

    delimDepth = (delim.height + delim.depth) - ruleWidth

    # Adjust the clearance based on the delimiter size
    if delimDepth > inner.height + inner.depth + lineClearance:
        lineClearance = (lineClearance + delimDepth - inner.height - inner.depth) / 2

    # Shift the delimiter so that its top lines up with the top of the line
    delimShift = -(inner.height + lineClearance + ruleWidth) + delim.height
    delim.style.top = em(delimShift)
    delim.height -= delimShift
    delim.depth += delimShift

    # We add a special case here, because even when `inner` is empty, we
    # still get a line. So, we use a simple heuristic to decide if we
    # should omit the body entirely. (note this doesn't work for something
    # like `\sqrt{\rlap{x}}`, but if someone is doing that they deserve for
    # it not to work.
    if inner.height == 0 and inner.depth == 0:
        body = makeSpan()
    else:
        body = buildCommon.makeVList([
            {"type": "elem", "elem": inner},
            {"type": "kern", "size": lineClearance},
            {"type": "elem", "elem": line},
            {"type": "kern", "size": ruleWidth},
        ], "firstBaseline", 0, options)

    if not group.value.index:
        return makeSpan(["mord", "sqrt"], [delim, body], options)
    else:
        # Handle the optional root index

        # The index is always in scriptscript style
        newOptions = options.withStyle(Style.SCRIPTSCRIPT)
        root = buildGroup(group.value.index, newOptions)
        rootWrap = makeSpan([style.reset(), Style.SCRIPTSCRIPT.cls()], [root], newOptions)

        # Figure out the height and depth of the inner part
        innerRootHeight = max(delim.height, body.height)
        innerRootDepth = max(delim.depth, body.depth)

        # The amount the index is shifted by. This is taken from the TeX
        # source, in the definition of `\r@@t`.
        toShift = 0.6 * (innerRootHeight - innerRootDepth)

        # Build a VList with the superscript shifted up correctly
        rootVList = buildCommon.makeVList(
            [{"type": "elem", "elem": rootWrap}],
            "shift", -toShift, options)
        # Add a class surrounding it so we can add on the appropriate
        # kerning
        rootVListWrap = makeSpan(["root"], [rootVList])

        return makeSpan(["mord", "sqrt"], [rootVListWrap, delim, body], options)
groupTypes['sqrt'] = groupSqrt

def groupSizing(group, options, prev):
    # Handle sizing operators like \Huge. Real TeX doesn't actually allow
    # these functions inside of math expressions, so we do some special
    # handling.
    inner = buildExpression(group.value.value, options.withSize(group.value.size), prev)

    # Compute the correct maxFontSize.
    style = options.style
    fontSize = buildCommon.sizingMultiplier[group.value.size]
    fontSize = fontSize * style.sizeMultiplier

    # Add size-resetting classes to the inner list and set maxFontSize
    # manually. Handle nested size changes.
    for innerEl in inner:
        if "sizing" not in innerEl.classes:
            innerEl.classes.extend(["sizing", "reset-" + options.size, group.value.size, style.cls()])
            innerEl.maxFontSize = fontSize
        else:
            pos = innerEl.classes.index("sizing")
            if innerEl.classes[pos + 1] == "reset-" + group.value.size) {
                # This is a nested size change: e.g., inner[i] is the "b" in
                # `\Huge a \small b`. Override the old size (the `reset-` class)
                # but not the new size.
                innerEl.classes[pos + 1] = "reset-" + options.size

    return buildCommon.makeFragment(inner)
groupTypes['sizing'] = groupSizing

def groupStyling(group, options, prev):
    # Style changes are handled in the TeXbook on pg. 442, Rule 3.

    # Figure out what style we're changing to.
    styleMap = {
        "display": Style.DISPLAY,
        "text": Style.TEXT,
        "script": Style.SCRIPT,
        "scriptscript": Style.SCRIPTSCRIPT,
    }

    newStyle = styleMap[group.value.style]
    newOptions = options.withStyle(newStyle)

    # Build the inner expression in the new style.
    inner = buildExpression(group.value.value, newOptions, prev)

    # Add style-resetting classes to the inner list. Handle nested changes.
    for innerEl in inner:
        if newStyle.reset() not in innerEl.classes:
            innerEl.classes.extend([options.style.reset(), newStyle.cls()])
        else:
            pos = innerEl.classes.index(newStyle.reset())
            # This is a nested style change, as `\textstyle a\scriptstyle b`.
            # Only override the old style (the reset class).
            innerEl.classes[pos] = options.style.reset()

    return new buildCommon.makeFragment(inner)
groupTypes['styling'] = groupStyling

def groupFont(group, options, prev):
    font = group.value.font
    return buildGroup(group.value.body, options.withFont(font), prev)
groupTypes['font'] = groupFont

def groupDelimSizing(group, options, prev):
    delim = group.value.value

    if delim == ".":
        # Empty delimiters still count as elements, even though they don't
        # show anything.
        return makeSpan([groupToType[group.value.delimType]])

    # Use delimiter.sizedDelim to generate the delimiter.
    return makeSpan(
        [groupToType[group.value.delimType]],
        [delimiter.sizedDelim(
            delim, group.value.size, options, group.mode)],
        options)
groupTypes['delimsizing'] = groupDelimSizing

def groupLeftRight(group, options, prev):
    # Build the inner expression
    inner = buildExpression(group.value.body, options.reset())

    innerHeight = 0
    innerDepth = 0

    # Calculate its height and depth
    for el in inner:
        innerHeight = max(el.height, innerHeight)
        innerDepth = max(el.depth, innerDepth)

    style = options.style

    # The size of delimiters is the same, regardless of what style we are
    # in. Thus, to correctly calculate the size of delimiter we need around
    # a group, we scale down the inner size based on the size.
    innerHeight *= style.sizeMultiplier
    innerDepth *= style.sizeMultiplier

    if group.value.left == ".":
        # Empty delimiters in \left and \right make null delimiter spaces.
        leftDelim = makeNullDelimiter(options)
    else:
        # Otherwise, use leftRightDelim to generate the correct sized
        # delimiter.
        leftDelim = delimiter.leftRightDelim(
            group.value.left, innerHeight, innerDepth, options,
            group.mode)
    # Add it to the beginning of the expression
    inner.unshift(leftDelim)

    # Same for the right delimiter
    if group.value.right == ".":
        rightDelim = makeNullDelimiter(options)
    else:
        rightDelim = delimiter.leftRightDelim(
            group.value.right, innerHeight, innerDepth, options,
            group.mode)
    # Add it to the end of the expression.
    inner.push(rightDelim)

    return makeSpan(["minner", style.cls()], inner, options)
groupTypes['leftright'] = groupLeftRight

def groupRule(group, options, prev):
    # Make an empty span for the rule
    rule = makeSpan(["mord", "rule"], [], options)
    style = options.style

    # Calculate the shift, width, and height of the rule, and account for units
    shift = 0
    if group.value.shift:
        shift = group.value.shift.number
        if group.value.shift.unit == "ex":
            shift *= style.metrics.xHeight

    width = group.value.width.number
    if group.value.width.unit == "ex":
        width *= style.metrics.xHeight

    height = group.value.height.number
    if group.value.height.unit == "ex":
        height *= style.metrics.xHeight

    # The sizes of rules are absolute, so make it larger if we are in a
    # smaller style.
    shift /= style.sizeMultiplier
    width /= style.sizeMultiplier
    height /= style.sizeMultiplier

    # Style the rule to the right size
    rule.style.borderRightWidth = em(width)
    rule.style.borderTopWidth = em(height)
    rule.style.bottom = em(shift)

    # Record the height and width
    rule.width = width
    rule.height = height + shift
    rule.depth = -shift

    return rule
groupTypes['rule'] = groupRule

def groupKern(group, options, prev):
    # Make an empty span for the rule
    rule = makeSpan(["mord", "rule"], [], options)
    style = options.style

    dimension = 0
    if group.value.dimension:
        dimension = group.value.dimension.number
        if group.value.dimension.unit == "ex":
            dimension *= style.metrics.xHeight

    dimension /= style.sizeMultiplier

    rule.style.marginLeft = em(dimension)

    return rule
groupTypes['kern'] = groupKern

def groupAccent(group, options, prev):
    # Accents are handled in the TeXbook pg. 443, rule 12.
    base = group.value.base
    style = options.style

    if group.type == "supsub":
        # If our base is a character box, and we have superscripts and
        # subscripts, the supsub will defer to us. In particular, we want
        # to attach the superscripts and subscripts to the inner body (so
        # that the position of the superscripts and subscripts won't be
        # affected by the height of the accent). We accomplish this by
        # sticking the base of the accent into the base of the supsub, and
        # rendering that, while keeping track of where the accent is.

        # The supsub group is the group that was passed in
        supsub = group
        # The real accent group is the base of the supsub group
        group = supsub.value.base
        # The character box is the base of the accent group
        base = group.value.base
        # Stick the character box into the base of the supsub group
        supsub.value.base = base

        # Rerender the supsub group with its new base, and store that
        # result.
        supsubGroup = buildGroup(supsub, options.reset(), prev)

    # Build the base group
    var body = buildGroup(base, options.withStyle(style.cramp()))

    # Calculate the skew of the accent. This is based on the line "If the
    # nucleus is not a single character, let s = 0 otherwise set s to the
    # kern amount for the nucleus followed by the \skewchar of its font."
    # Note that our skew metrics are just the kern between each character
    # and the skewchar.
    if isCharacterBox(base):
        # If the base is a character box, then we want the skew of the
        # innermost character. To do that, we find the innermost character:
        baseChar = getBaseElem(base)
        # Then, we render its group to get the symbol inside it
        baseGroup = buildGroup(baseChar, options.withStyle(style.cramp()))
        # Finally, we pull the skew off of the symbol.
        skew = baseGroup.skew
        # Note that we now throw away baseGroup, because the layers we
        # removed with getBaseElem might contain things like \color which
        # we can't get rid of.
        # TODO(emily): Find a better way to get the skew
    else:
        skew = 0

    # calculate the amount of space between the body and the accent
    clearance = min(body.height, style.metrics.xHeight)

    # Build the accent
    accent = buildCommon.makeSymbol(group.value.accent, "Main-Regular", "math", options)
    # Remove the italic correction of the accent, because it only serves to
    # shift the accent over to a place we don't want.
    accent.italic = 0

    # The \vec character that the fonts use is a combining character, and
    # thus shows up much too far to the left. To account for this, we add a
    # specific class which shifts the accent over to where we want it.
    # TODO(emily): Fix this in a better way, like by changing the font
    vecClass = "accent-vec" if group.value.accent == "\\vec" else None

    accentBody = makeSpan(["accent-body", vecClass], [makeSpan([], [accent])])

    accentBody = buildCommon.makeVList([
        {"type": "elem", "elem": body},
        {"type": "kern", "size": -clearance},
        {"type": "elem", "elem": accentBody},
    ], "firstBaseline", 0, options)

    # Shift the accent over by the skew. Note we shift by twice the skew
    # because we are centering the accent, so by adding 2*skew to the left,
    # we shift it to the right by 1*skew.
    accentBody.children[1].style.marginLeft = em(2 * skew)

    accentWrap = makeSpan(["mord", "accent"], [accentBody], options)

    if supsubGroup:
        # Here, we replace the "base" child of the supsub with our newly
        # generated accent.
        supsubGroup.children[0] = accentWrap

        # Since we don't rerun the height calculation after replacing the
        # accent, we manually recalculate height.
        supsubGroup.height = max(accentWrap.height, supsubGroup.height)

        # Accents should always be ords, even when their innards are not.
        supsubGroup.classes[0] = "mord"

        return supsubGroup
    else:
        return accentWrap
groupTypes['accent'] = groupAccent

def groupPhantom(group, options, prev):
    elements = buildExpression(
        group.value.value,
        options.withPhantom(),
        prev
    )

    # \phantom isn't supposed to affect the elements it contains.
    # See "color" for more details.
    return new buildCommon.makeFragment(elements)
groupTypes['phantom'] = groupPhantom

# buildGroup is the function that takes a group and calls the correct groupType
# function for it. It also handles the interaction of size and style changes
# between parents and children.
def buildGroup(group, options, prev):
    if not group:
        return makeSpan()

    if group.type in groupTypes:
        # Call the groupTypes function
        groupNode = groupTypes[group.type](group, options, prev)

        # If the style changed between the parent and the current group,
        # account for the size difference
        if options.style != options.parentStyle:
            multiplier = options.style.sizeMultiplier / options.parentStyle.sizeMultiplier

            groupNode.height *= multiplier
            groupNode.depth *= multiplier

        # If the size changed between the parent and the current group, account
        # for that size difference.
        if options.size != options.parentSize:
            multiplier = buildCommon.sizingMultiplier[options.size] / buildCommon.sizingMultiplier[options.parentSize]

            groupNode.height *= multiplier
            groupNode.depth *= multiplier

        return groupNode
    else:
        die("Got group of unknown type: '" + group.type + "'")


# Take an entire parse tree, and build it into an appropriate set of HTML
# nodes.
def buildHTML(tree, options):
    # buildExpression is destructive, so we need to make a clone
    # of the incoming tree so that it isn't accidentally changed
    import copy
    tree = copy.deepcopy(tree)

    # Build the expression contained in the tree
    expression = buildExpression(tree, options)
    body = makeSpan(["base", options.style.cls()], expression, options)

    # Add struts, which ensure that the top of the HTML element falls at the
    # height of the expression, and the bottom of the HTML element falls at the
    # depth of the expression.
    topStrut = makeSpan(["strut"])
    bottomStrut = makeSpan(["strut", "bottom"])

    topStrut.style.height = em(body.height)
    bottomStrut.style.height = em(body.height + body.depth)

    # We'd like to use `vertical-align: top` but in IE 9 this lowers the
    # baseline of the box to the bottom of this strut (instead staying in the
    # normal place) so we use an absolute value for vertical-align instead
    bottomStrut.style.verticalAlign = em(-body.depth)

    # Wrap the struts and body together
    htmlNode = makeSpan(["katex-html"], [topStrut, bottomStrut, body])

    htmlNode.setAttribute("aria-hidden", "true")

    return htmlNode
