-- | Render 'SimpleDocStream' in a terminal.
module Prettyprinter.Render.Terminal (
    -- * Styling
    AnsiStyle,
    Color(..),

    -- ** Font color
    color, colorDull, palette,

    -- ** Background color
    bgColor, bgColorDull, bgPalette,

    -- ** Font style
    bold, italicized, underlined,

    -- ** Internal markers
    --
    -- | These should only be used for writing adaptors to other libraries; for
    -- the average use case, use 'bold', 'bgColorDull', etc.
    Intensity,
    ColorIntensity(..),
    Bold(..),
    Underlined(..),
    Italicized(..),

    -- * Conversion to ANSI-infused 'Text'
    renderLazy, renderStrict,

    -- * Render directly to 'stdout'
    renderIO,

    -- ** Convenience functions
    putDoc, hPutDoc,
) where

import Prettyprinter.Render.Terminal.Internal
