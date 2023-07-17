version = "0.21.2"

xplr.config.modes.custom.quick = {
  name = "quick",
  key_bindings = {
    on_key = {
      l = {
        help = "Sort by Last Modified (desc)",
        messages = {
          "ClearNodeSorters",
          { AddNodeSorter = { sorter = "ByLastModified", reverse = false } },
          "ExplorePwd",
          "PopMode",
        },
      },
      L = {
        help = "Sort by Last Modified (asc)",
        messages = {
          "ClearNodeSorters",
          { AddNodeSorter = { sorter = "ByLastModified", reverse = true } },
          "ExplorePwd",
          "PopMode",
        },
      },
      p = {
        help = "Sort by Relative Path (desc)",
        messages = {
          "ClearNodeSorters",
          { AddNodeSorter = { sorter = "ByIRelativePath", reverse = false } },
          "ExplorePwd",
          "PopMode",
        },
      },
      P = {
        help = "Sort by Relative Path (asc)",
        messages = {
          "ClearNodeSorters",
          { AddNodeSorter = { sorter = "ByIRelativePath", reverse = true } },
          "ExplorePwd",
          "PopMode",
        },
      },
      r = {
        help = "Sort Reset",
        messages = {
          "ClearNodeSorters",
          { AddNodeSorter = { sorter = "ByCanonicalIsDir", reverse = true } },
          { AddNodeSorter = { sorter = "ByIRelativePath", reverse = false } },
          "ExplorePwd",
          "PopMode",
        }
      },
    },
    default = {
      messages = {
        "PopMode",
      },
    },
  },
}

xplr.config.modes.builtin.default.key_bindings.on_key["z"] = {
  help = "quick",
  messages = {
    { SwitchModeCustom = "quick" },
  },
}

local home = os.getenv("HOME")
package.path = home
.. "/.config/xplr/plugins/?/init.lua;"
.. home
.. "/.config/xplr/plugins/?.lua;"
.. package.path

require("fzf").setup()
require("tree-view").setup({ toggle_layout_key = "T", })
require("xclip").setup()
