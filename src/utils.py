# This file contains a list of utility functions which are useful in other
# files.

# Converts camelCase to kebab-case
firstCapRe = re.compile('(.)([A-Z][a-z]+)')
allCapRe = re.compile('([a-z0-9])([A-Z])')
def hyphenate(name):
    s1 = firstCapRe.sub(r'\1-\2', name)
    return allCapRe.sub(r'\1-\2', s1).lower()

# Escapes text to prevent scripting attacks.
#
# @param {*} text Text value to escape.
# @return {string} An escaped string.
def escape(text):
    return (text
        .replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
        .replace("'", "&apos;").replace('"', "&quot;")
        )
