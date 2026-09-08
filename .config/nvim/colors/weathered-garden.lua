vim.o.termguicolors = true
vim.o.background = "dark"
vim.cmd.highlight "clear"
vim.g.colors_name = "weathered-garden"

local c = {
  black =       "#222020",
  white =       "#D8CFC4",
  light_gray =  "#C6B8AA",
  medium_gray = "#92917B",
  dark_gray =   "#393535",
  red =         "#D45C4F",
  green =       "#979B72",
  yellow =      "#D1B078",
  blue =        "#91ABC4",
  magenta =     "#C58F9D",
  cyan =        "#81B4B0",
  maroon =      "#A17F6C",
  debug =       "#EF7565",
}

local function set(opts)
  local name = opts[1]
  table.remove(opts, 1)
  vim.api.nvim_set_hl(0, name, opts)
end

set { "Normal",    fg = c.white, bg = c.black }
set { "Comment",   fg = c.maroon }
set { "Constant",  fg = c.magenta }
set { "Number",    fg = c.blue }
set { "Special",   fg = c.cyan }
set { "String",    fg = c.green }

set { "Function",   fg = c.white }
set { "Identifier", fg = c.white }

local syntax = {
  "Statement",
  "Conditional",
  "Repeat",
  "Label",
  "Operator",
  "Keyword",
  "Exception",

  "PreProc",
  "Include",
  "Define",
  "Macro",
  "PreCondit",

  "Type",
  "StorageClass",
  "Structure",
  "Typedef",

  "Delimiter",
}
for _, name in ipairs(syntax) do
  set { name, fg = c.light_gray }
end

set { "StatusLine", bg = c.dark_gray }

set { "DiagnosticError", fg = c.red }
set { "DiagnosticHint",  fg = c.cyan }
set { "DiagnosticInfo",  fg = c.blue }
set { "DiagnosticOk",    fg = c.green }
set { "DiagnosticWarn",  fg = c.yellow }

-- Language-specific

vim.g.python_constant_highlight = true
set { "pythonBoolean", fg = c.magenta }

-- Plugin-specific

set { "OilEmpty",     fg = c.light_gray }
set { "OilHidden",    fg = c.light_gray }
set { "OilDir",       fg = c.white }
set { "OilDirHidden", fg = c.light_gray }
