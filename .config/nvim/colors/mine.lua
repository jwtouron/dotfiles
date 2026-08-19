vim.o.termguicolors = true
vim.o.background = "dark"
vim.cmd.highlight "clear"
vim.g.colors_name = "mine"

local c = {
  black =       "#1d2328",
  white =       "#e1e7e9",
  -- light_gray =  "#c0c8cc",
  light_gray =  "#b8c1c5",
  -- light_gray =  "#aeb8bd",
  -- light_gray =  "#a4afb4",
  medium_gray = "#747d82",
  dark_gray =   "#30383e",
  red =         "#d36f72",
  green =       "#68c174",
  yellow =      "#d0b66a",
  blue =        "#5eacd3",
  magenta =     "#b889d3",
  cyan =        "#63c0ba",
  maroon =      "#925359",
  -- maroon =      "#824a4f",
  -- maroon =      "#7a464b",
  -- maroon =      "#704146",
  -- maroon =      "#663c41",
  debug =       "#FF0000",
}

local function set(opts)
  local name = opts[1]
  table.remove(opts, 1)
  vim.api.nvim_set_hl(0, name, opts)
end

set { "Normal",    fg = c.white, bg = c.black }
set { "Comment",   fg = c.maroon, }
set { "Constant",  fg = c.magenta }
set { "Number",    fg = c.blue }
set { "Special",   fg = c.cyan }
set { "String",    fg = c.green }
-- set { "@string.lua",    fg = c.green, priority = 50 }

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

-- Plugin-specific

set { "OilEmpty",     fg = c.light_gray }
set { "OilHidden",    fg = c.light_gray }
set { "OilDir",       fg = c.white }
set { "OilDirHidden", fg = c.light_gray }
