
# This file contains information about the options that the Parser carries
# around with it while parsing. Data is held in an `Options` object, and when
# recursing, a new `Options` object can be created with the `.with*` and
# `.reset` functions.

# This is the main options class. It contains the style, size, color, and font
# of the current parse level. It also contains the style and size of the parent
# parse level, so size changes can be handled efficiently.
#
# Each of the `.with*` and `.reset` functions passes its current style and size
# as the parentStyle and parentSize of the new options class, so parent
# handling is taken care of automatically.
class Options(object):
    def __init__(self, style=None, color=None, size=None, phantom=None, font=None, parentStyle=None, parentSize=None):
        self.style = style
        self.color = color
        self.size = size
        self.phantom = phantom
        self.font = font

        self.parentStyle = parentStyle or style
        self.parentSize = parentSize or size

    # Returns a new options object with the same properties as "this".  Properties
    # from "extension" will be copied to the new options object.
    def extend(self, style=None, color=None, size=None, phantom=None, font=None, parentStyle=None, parentSize=None):
        return new Options(
            style = style or self.style,
            color = color or self.color,
            size = size or self.size,
            phantom = phantom or self.phantom,
            font = font or self.font,
            parentStyle = parentStyle or self.parentStyle,
            parentSize = parentSize or self.parentSize
            )

    # Create a new options object with the same style, size, and color. This is
    # used so that parent style and size changes are handled correctly.
    def reset(self):
        return self.extend()

    # Gets the CSS color of the current options object, accounting for the `colorMap`.
    def getColor(self):
        if self.phantom:
            return "transparent"
        return colorMap.get(self.color, self.color)


# A map of color names to CSS colors.
# TODO(emily): Remove this when we have real macros
colorMap = {
    "katex-blue": "#6495ed",
    "katex-orange": "#ffa500",
    "katex-pink": "#ff00af",
    "katex-red": "#df0030",
    "katex-green": "#28ae7b",
    "katex-gray": "gray",
    "katex-purple": "#9d38bd",
    "katex-blueA": "#ccfaff",
    "katex-blueB": "#80f6ff",
    "katex-blueC": "#63d9ea",
    "katex-blueD": "#11accd",
    "katex-blueE": "#0c7f99",
    "katex-tealA": "#94fff5",
    "katex-tealB": "#26edd5",
    "katex-tealC": "#01d1c1",
    "katex-tealD": "#01a995",
    "katex-tealE": "#208170",
    "katex-greenA": "#b6ffb0",
    "katex-greenB": "#8af281",
    "katex-greenC": "#74cf70",
    "katex-greenD": "#1fab54",
    "katex-greenE": "#0d923f",
    "katex-goldA": "#ffd0a9",
    "katex-goldB": "#ffbb71",
    "katex-goldC": "#ff9c39",
    "katex-goldD": "#e07d10",
    "katex-goldE": "#a75a05",
    "katex-redA": "#fca9a9",
    "katex-redB": "#ff8482",
    "katex-redC": "#f9685d",
    "katex-redD": "#e84d39",
    "katex-redE": "#bc2612",
    "katex-maroonA": "#ffbde0",
    "katex-maroonB": "#ff92c6",
    "katex-maroonC": "#ed5fa6",
    "katex-maroonD": "#ca337c",
    "katex-maroonE": "#9e034e",
    "katex-purpleA": "#ddd7ff",
    "katex-purpleB": "#c6b9fc",
    "katex-purpleC": "#aa87ff",
    "katex-purpleD": "#7854ab",
    "katex-purpleE": "#543b78",
    "katex-mintA": "#f5f9e8",
    "katex-mintB": "#edf2df",
    "katex-mintC": "#e0e5cc",
    "katex-grayA": "#f6f7f7",
    "katex-grayB": "#f0f1f2",
    "katex-grayC": "#e3e5e6",
    "katex-grayD": "#d6d8da",
    "katex-grayE": "#babec2",
    "katex-grayF": "#888d93",
    "katex-grayG": "#626569",
    "katex-grayH": "#3b3e40",
    "katex-grayI": "#21242c",
    "katex-kaBlue": "#314453",
    "katex-kaGreen": "#71B307",
}
