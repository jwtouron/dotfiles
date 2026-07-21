require "conf.keybinds"

------------------
---- MONITORS ----
------------------

hl.monitor({
  output   = "",
  mode     = "preferred",
  position = "auto",
  scale    = 1,
})


---------------------
---- MY PROGRAMS ----
---------------------

-- Set programs that you use
--local terminal    = "kitty"
--local fileManager = "dolphin"
--local menu        = "hyprlauncher"


-------------------
---- AUTOSTART ----
-------------------

hl.on("hyprland.start", function ()
  hl.exec_cmd("noctalia")
  hl.exec_cmd("~/.config/waypaper/init.sh")
--   hl.exec_cmd("waybar & hyprpaper & firefox")
end)


-----------------------
---- LOOK AND FEEL ----
-----------------------

-- Refer to https://wiki.hypr.land/Configuring/Basics/Variables/
hl.config({
  general = {
    border_size = 2,

    col = {
      active_border = {
        -- fire: https://www.schemecolor.com/fire-color-scheme.php
        colors = {
          "rgb(FAC000)",
          "rgb(FF7500)",
          "rgb(FC6400)",
          "rgb(D73502)",
          "rgb(B62203)",
          "rgb(801100)",
        },
        angle = 45
      },
    },

    resize_on_border = true,
  },

  decoration = {
    rounding = 10,

    shadow = {
      enabled = true,
      -- sharp = true,
    },
  },

  group = {
    col = {
      border_active = {
        colors = {
          "rgb(FAC000)",
          "rgb(FF7500)",
          "rgb(FC6400)",
          "rgb(D73502)",
          "rgb(B62203)",
          "rgb(801100)",
        },
        angle = 45
      }
    },

    groupbar = {
      col = {
        active = "rgb(FAC000)",
      },
      font_size = 0,
      height = 0,
      keep_upper_gap = false,
    },
  },

  misc = {
    disable_hyprland_logo = true,
  },
})

--------------------------------
---- ANIMATIONS ----------------
--------------------------------

hl.curve("easeOutQuint",   { type = "bezier", points = { {0.23, 1},    {0.32, 1}    } })

hl.animation({ leaf = "global", enabled = true,  speed = 5, bezier = "easeOutQuint" })

--------------------------------
---- WINDOWS AND WORKSPACES ----
--------------------------------

hl.window_rule({
  -- Ignore maximize requests from all apps. You'll probably like this.
  name  = "suppress-maximize-events",
  match = { class = ".*" },
  suppress_event = "maximize",
})
