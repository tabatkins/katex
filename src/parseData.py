# -*- coding: utf-8 -*-

from __future__ import division, unicode_literals

# The resulting parse tree nodes of the parse tree.
#
# It is possible to provide position information, so that a ParseNode can
# fulfil a role similar to a Token in error reporting.
# For details on the corresponding properties see Token constructor.
# Providing such information can lead to better error reporting.
#
# @param {string}  type       type of node, like e.g. "ordgroup"
# @param {?object} value      type-specific representation of the node
# @param {string}  mode       parse mode in action for this node,
#                             "math" or "text"
# @param {Token=} firstToken  first token of the input for this node,
#                             will omit position information if unset
# @param {Token=} lastToken   last token of the input for this node,
#                             will default to firstToken if unset
class ParseNode(object):
    def __init__(self, type, value, mode, firstToken=None, lastToken=None):
        self.type = type;
        self.value = value;
        self.mode = mode;
        if firstToken and (not lastToken or lastToken.lexer == firstToken.lexer):
            self.lexer = firstToken.lexer;
            self.start = firstToken.start;
            self.end = (lastToken or firstToken).end;
