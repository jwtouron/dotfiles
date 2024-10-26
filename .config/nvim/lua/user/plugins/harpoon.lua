local function desc(s)
  return "Harpoon: "..s
end

local function mark(f)
  return "<cmd>lua require('harpoon.mark')." .. f .. "<cr>"
end

local function ui(f)
  return "<cmd>lua require('harpoon.ui')." .. f .. "<cr>"
end

return {
  'ThePrimeagen/harpoon',
  enabled = false,
  dependencies = 'nvim-lua/plenary.nvim',
  keys = {
    { "<A-a>", mark("add_file()"),        desc = desc("Add File") },
    { "<A-m>", ui("toggle_quick_menu()"), desc = desc("Toggle Quick Menu") },
    { "<A-l>", ui('nav_next()'),          desc = desc("Nav Next") },
    { "<A-h>", ui('nav_prev()'),          desc = desc("Nav Prev") },
    { "<A-1>", ui("nav_file(1)"),         desc = desc("Nav File 1") },
    { "<A-2>", ui("nav_file(2)"),         desc = desc("Nav File 2") },
    { "<A-3>", ui("nav_file(3)"),         desc = desc("Nav File 3") },
    { "<A-4>", ui("nav_file(4)"),         desc = desc("Nav File 4") },
    { "<A-5>", ui("nav_file(5)"),         desc = desc("Nav File 5") },
    { "<A-6>", ui("nav_file(6)"),         desc = desc("Nav File 6") },
  },
}
