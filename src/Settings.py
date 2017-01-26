# This is a module for storing settings passed into KaTeX. It correctly handles
# default settings.

# The main Settings object
#
# The current options stored are:
#  - displayMode: Whether the expression should be typeset by default in
#                 textstyle or displaystyle (default false=displaystyle)
class Settings(object):
	def __init__(self, displayMode=False, throwOnError=True, errorColor="#cc0000", macros=None):
		self.displayMode = displayMode
		self.throwOnError = throwOnError
		self.errorColor = errorColor
		self.macros = macros or {}
