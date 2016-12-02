# -*- coding: utf-8 -*-

from __future__ import division, unicode_literals

# This is the main entry point for KaTeX. Here, we expose functions for
# rendering expressions either to DOM nodes or to markup strings.
#
# We also expose the ParseError class to check if errors thrown from KaTeX are
# errors in the expression, or errors in javascript handling.

from .ParseError import ParseError
from .Settings import Settings
from .buildTree import buildTree
from .parseTree import parseTree
from . import utils

def render(expr, container, options):
    '''
    Parse and build an expression, and place that expression in the DOM node given.
    '''
    utils.clearContents(container)

    settings = Settings(options)

    tree = parseTree(expr, settings)
    node = buildTree(tree, expr, settings).toNode()

    appendChild(container, node)

def renderToString(expr, options):
    '''
    Parse and build an expression, and return the markup for that.
    '''
    settings = Settings(options)
    tree = parseTree(expr, settings)
    return buildTree(tree, expr, settings).toMarkup()

def _generateParseTree(expr, options):
    '''
    Parse an expression and return the parse tree.
    '''
    settings = Settings(options)
    return parseTree(expr, settings)
