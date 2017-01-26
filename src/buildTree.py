# -*- coding: utf-8 -*-

from __future__ import division, unicode_literals

from . import buildHTML
from . import buildMathML
from . import buildCommon
from . import Options
from . import Settings
from . import Style
from .buildCommon import makeSpan

def buildTree(tree, expression, settings=None):
    if settings is None:
        settings = Settings()

    startStyle = Style.TEXT
    if settings.displayMode:
        startStyle = Style.DISPLAY

    # Setup the default options
    options = Options(style=startStyle, size="size5")

    # `buildHTML` sometimes messes with the parse tree (like turning bins ->
    # ords), so we build the MathML version first.
    mathMLNode = buildMathML(tree, expression, options)
    htmlNode = buildHTML(tree, options)

    katexNode = makeSpan(["katex"], [mathMLNode, htmlNode])

    if settings.displayMode:
        return makeSpan(["katex-display"], [katexNode])
    else:
        return katexNode
