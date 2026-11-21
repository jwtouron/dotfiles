local palettes = {
  ["neon-skyline"] = {
    black      = "#10101e",
    white      = "#e8e6ff",
    light_gray = "#c3bedb",
    dark_gray  = "#25233d",
    red        = "#ff537a",
    green      = "#62ffb0",
    yellow     = "#ffe66d",
    blue       = "#57a5ff",
    magenta    = "#ff5cde",
    cyan       = "#36f9f6",
    comment    = "#807aab",
    -- comment =      "#9273a6",
  },

  ["rainlit-slate"] = {
    black =       "#1d2328",
    white =       "#e1e7e9",
    -- light_gray =  "#c0c8cc",
    light_gray =  "#b8c1c5",
    -- light_gray =  "#aeb8bd",
    -- light_gray =  "#a4afb4",
    dark_gray =   "#30383e",
    red =         "#d36f72",
    green =       "#68c174",
    yellow =      "#d0b66a",
    blue =        "#5eacd3",
    magenta =     "#b889d3",
    cyan =        "#63c0ba",
    comment =      "#925359",
    -- comment =      "#824a4f",
    -- comment =      "#7a464b",
    -- comment =      "#704146",
    -- comment =      "#663c41",
    debug =       "#FF0000",
  },

  ["weathered-garden"] = {
    black =       "#222020",
    white =       "#D8CFC4",
    light_gray =  "#C6B8AA",
    dark_gray =   "#393535",
    red =         "#D45C4F",
    green =       "#979B72",
    yellow =      "#D1B078",
    blue =        "#91ABC4",
    magenta =     "#C58F9D",
    cyan =        "#81B4B0",
    comment =     "#A17F6C",
    debug =       "#EF7565",
  },
}

local function set(opts)
  local name = opts[1]
  table.remove(opts, 1)
  vim.api.nvim_set_hl(0, name, opts)
end

local M = {}

function M.apply(name)
  local c = palettes[name]

  vim.o.termguicolors = true
  vim.o.background = "dark"
  vim.cmd.highlight "clear"
  vim.g.colors_name = name

  set { "Normal",    fg = c.white, bg = c.black }
  set { "Comment",   fg = c.comment }
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
  for _, n in ipairs(syntax) do
    set { n, fg = c.light_gray }
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
end

return M
