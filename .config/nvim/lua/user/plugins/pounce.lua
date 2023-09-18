return {
  "rlane/pounce.nvim",
  keys = {
    { "s", function() require'pounce'.pounce { } end,                              desc = "Pounce" },
    { "S", function() require'pounce'.pounce { do_repeat = true } end,             desc = "Pounce (repeat)" },
    { "s", function() require'pounce'.pounce { } end,                  mode = "x", desc = "Pounce" },
  },
}
