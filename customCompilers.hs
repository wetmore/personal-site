module CustomCompilers (pandocMathCompiler) where

import qualified Data.Set as S
import           Control.Applicative
import           Hakyll
import           Text.Pandoc

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                          Ext_latex_macros, Ext_inline_code_attributes]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr S.insert defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""
                        }
    in sidenoteCompilerWith defaultHakyllReaderOptions writerOptions

sidenoteCompilerWith :: ReaderOptions -> WriterOptions -> Compiler (Item String)
sidenoteCompilerWith = customWriterCompilerWith $ writeCustom "sidenote.lua"


customWriterCompilerWith :: (WriterOptions -> Pandoc -> IO String)
                         -> ReaderOptions -> WriterOptions
                         -> Compiler (Item String)
customWriterCompilerWith customWriter ropt wopt = do
    body <- getResourceBody
    pandoc <- readPandocWith ropt body
    withItemBody (unsafeCompiler . customWriter wopt) pandoc