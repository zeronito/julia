# -*- coding: utf-8 -*-
#
# Julia Language documentation build configuration file, created by
# sphinx-quickstart on Sat Apr 14 22:49:22 2012.
#
# This file is execfile()d with the current directory set to its containing dir.
#
# Note that not all possible configuration values are present in this
# autogenerated file.
#
# All configuration values have a default; values that are commented out
# serve to show the default.

import sys, os, re

import juliadoc
import sphinx_rtd_theme

# -- General configuration -----------------------------------------------------

# If your documentation needs a minimal Sphinx version, state it here.
#needs_sphinx = '1.0'

# Add any Sphinx extension module names here, as strings. They can be extensions
# coming with Sphinx (named 'sphinx.ext.*') or your custom ones.
extensions = ['sphinx.ext.mathjax',
              'juliadoc.julia',
              'juliadoc.jldoctest',
              'juliadoc.jlhelp']

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# The suffix of source filenames.
source_suffix = '.rst'

# The encoding of source files.
#source_encoding = 'utf-8-sig'

# The master toctree document.
master_doc = 'index'

# General information about the project.
project = u'Julia Language'
AUTHORS = u"Jeff Bezanson, Stefan Karpinski, Viral Shah, Alan Edelman, et al."
copyright = u'2012-2015, '+AUTHORS

# The version info for the project you're documenting, acts as replacement for
# |version| and |release|, also used in various other places throughout the
# built documents.
#
try:
    # The full version, including alpha/beta/rc tags.
    with open("../VERSION") as f:
        release = f.read().rstrip()
    # The short X.Y version.
    version = '.'.join(re.split('[.-]', release)[:2])
except:
    release = 'X.Y.Z-unknown'
    version = 'X.Y'

# The language for content autogenerated by Sphinx. Refer to documentation
# for a list of supported languages.
#language = None

# There are two options for replacing |today|: either, you set today to some
# non-false value, then it is used:
#today = ''
# Else, today_fmt is used as the format for a strftime call.
#today_fmt = '%B %d, %Y'

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
exclude_patterns = ['_build', 'manual/unicode-input-table.rst']

# The reST default role (used for this markup: `text`) to use for all documents.
#default_role = None

# If true, '()' will be appended to :func: etc. cross-reference text.
#add_function_parentheses = True

# If true, the current module name will be prepended to all description
# unit titles (such as .. function::).
add_module_names = False

# If true, sectionauthor and moduleauthor directives will be shown in the
# output. They are ignored by default.
#show_authors = False

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = 'sphinx'

# A list of ignored prefixes for module index sorting.
#modindex_common_prefix = []

primary_domain = 'jl'
highlight_language = 'julia'


# -- Options for HTML output ---------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
html_theme = 'julia'

# Theme options are theme-specific and customize the look and feel of a theme
# further.  For a list of options available for each theme, see the
# documentation.
#html_theme_options = {}

# Add any paths that contain custom themes here, relative to this directory.
html_theme_path = [juliadoc.get_theme_dir(),
                   sphinx_rtd_theme.get_html_theme_path()]

# The name for this set of Sphinx documents.  If None, it defaults to
# "<project> v<release> documentation".
#html_title = None

# A shorter title for the navigation bar.  Default is the same as html_title.
#html_short_title = None

# The name of an image file (relative to this directory) to place at the top
# of the sidebar.
#html_logo = None

# The name of an image file (within the static path) to use as favicon of the
# docs.  This file should be a Windows icon file (.ico) being 16x16 or 32x32
# pixels large.
#html_favicon = None

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = []

# If not '', a 'Last updated on:' timestamp is inserted at every page bottom,
# using the given strftime format.
#html_last_updated_fmt = '%b %d, %Y'

# If true, SmartyPants will be used to convert quotes and dashes to
# typographically correct entities.
#html_use_smartypants = True

# Custom sidebar templates, maps document names to template names.
#html_sidebars = {}

# Additional templates that should be rendered to pages, maps page names to
# template names.
#html_additional_pages = {}

# If false, no module index is generated.
html_domain_indices = False

# If false, no index is generated.
#html_use_index = True

# If true, the index is split into individual pages for each letter.
#html_split_index = False

# If true, links to the reST sources are added to the pages.
#html_show_sourcelink = True

# If true, "Created using Sphinx" is shown in the HTML footer. Default is True.
html_show_sphinx = False

# If true, "(C) Copyright ..." is shown in the HTML footer. Default is True.
html_show_copyright = False

# If true, an OpenSearch description file will be output, and all pages will
# contain a <link> tag referring to it.  The value of this option must be the
# base URL from which the finished HTML is served.
#html_use_opensearch = ''

# This is the file name suffix for HTML files (e.g. ".xhtml").
#html_file_suffix = None

# Output file base name for HTML help builder.
htmlhelp_basename = 'JuliaLanguageDoc'


# -- Options for LaTeX output --------------------------------------------------

latex_elements = {
# The paper size ('letterpaper' or 'a4paper').
#'papersize': 'letterpaper',

# The font size ('10pt', '11pt' or '12pt').
#'pointsize': '10pt',

# Additional stuff for the LaTeX preamble.
#'preamble': '',

    'utf8extra': r'''
        \usepackage{CJKutf8}
        \usepackage{amssymb}
        \DeclareUnicodeCharacter{00B9}{\ensuremath{^{1}}}
        \DeclareUnicodeCharacter{00A0}{\nobreakspace}
        \DeclareUnicodeCharacter{00D7}{\ensuremath{\times}}
        \DeclareUnicodeCharacter{00F7}{\ensuremath{\div}}
        \DeclareUnicodeCharacter{0127}{\ensuremath{\hbar}}
        \DeclareUnicodeCharacter{03B3}{\ensuremath{\gamma}}
        \DeclareUnicodeCharacter{03C0}{\ensuremath{\pi}}
        \DeclareUnicodeCharacter{03C6}{\ensuremath{\varphi}}
        \DeclareUnicodeCharacter{2081}{\ensuremath{_{1}}}
        \DeclareUnicodeCharacter{2203}{\ensuremath{\exists}}
        \DeclareUnicodeCharacter{2200}{\ensuremath{\forall}}
        \DeclareUnicodeCharacter{2208}{\ensuremath{\in}}
        \DeclareUnicodeCharacter{220B}{\ensuremath{\ni}}
        \DeclareUnicodeCharacter{2209}{\ensuremath{\notin}}
        \DeclareUnicodeCharacter{220C}{\ensuremath{\not\ni}}
        \DeclareUnicodeCharacter{2211}{\ensuremath{\sum}}
        \DeclareUnicodeCharacter{221A}{\ensuremath{\sqrt{}}}
        \DeclareUnicodeCharacter{221B}{\ensuremath{\sqrt[3]{}}}
        \DeclareUnicodeCharacter{222A}{\ensuremath{\cup}}
        \DeclareUnicodeCharacter{2229}{\ensuremath{\cap}}
        \DeclareUnicodeCharacter{2248}{\ensuremath{\approx}}
        \DeclareUnicodeCharacter{2249}{\ensuremath{\not\approx}}
        \DeclareUnicodeCharacter{2260}{\ensuremath{\ne}}
        \DeclareUnicodeCharacter{2261}{\ensuremath{\equiv}}
        \DeclareUnicodeCharacter{2262}{\ensuremath{\not\equiv}}
        \DeclareUnicodeCharacter{2264}{\ensuremath{\le}}
        \DeclareUnicodeCharacter{2265}{\ensuremath{\ge}}
        \DeclareUnicodeCharacter{2286}{\ensuremath{\subseteq}}
        \DeclareUnicodeCharacter{2288}{\ensuremath{\nsubseteq}}
        \DeclareUnicodeCharacter{228A}{\ensuremath{\subsetneq}}
        \DeclareUnicodeCharacter{2295}{\ensuremath{\oplus}}
        \DeclareUnicodeCharacter{2297}{\ensuremath{\otimes}}
        \DeclareUnicodeCharacter{22C5}{\ensuremath{\cdot}}
        \DeclareUnicodeCharacter{2713}{x}
        \DeclareUnicodeCharacter{27FA}{\ensuremath{\Longleftrightarrow}}
    ''',
}

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title, author, documentclass [howto/manual]).
latex_documents = [
  ('latex', 'JuliaLanguage.tex', u'Julia Language Documentation',
   AUTHORS, 'manual'),
]

# The name of an image file (relative to this directory) to place at the top of
# the title page.
#latex_logo = None

# For "manual" documents, if this is true, then toplevel headings are parts,
# not chapters.
latex_use_parts = True

# If true, show page references after internal links.
latex_show_pagerefs = True

# If true, show URL addresses after external links.
#latex_show_urls = False

# Documents to append as an appendix to all manuals.
#latex_appendices = []

# If false, no module index is generated.
latex_domain_indices = False


# -- Options for manual page output --------------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    ('index', 'julialanguage', u'Julia Language Documentation',
     [AUTHORS], 1)
]

# If true, show URL addresses after external links.
#man_show_urls = False


# -- Options for Texinfo output ------------------------------------------------

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
texinfo_documents = [
  ('index', 'JuliaLanguage', u'Julia Language Documentation',
   AUTHORS, 'JuliaLanguage', 'One line description of project.',
   'Miscellaneous'),
]

# Documents to append as an appendix to all manuals.
#texinfo_appendices = []

# If false, no module index is generated.
#texinfo_domain_indices = True

# How to display URL addresses: 'footnote', 'no', or 'inline'.
#texinfo_show_urls = 'footnote'
