local awful = require("awful")
local gears = require("gears")
local hotkeys_popup = require("awful.hotkeys_popup")
local sharedtags = require("sharedtags")
local volume_widget = require('awesome-wm-widgets.volume-widget.volume')

local keybindings = {}

local function init_global_keys(vars)
    local modkey = vars.modkey
    local terminal = vars.terminal
    local browser = vars.browser
    local logout_popup = vars.logout_popup
    return gears.table.join(
        awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
                {description="show help", group="awesome"}),
        awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
                {description = "view previous", group = "tag"}),
        awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
                {description = "view next", group = "tag"}),
        awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
                {description = "go back", group = "tag"}),

        awful.key({ modkey,           }, "j",
            function ()
                awful.client.focus.byidx( 1)
            end,
            {description = "focus next by index", group = "client"}
        ),
        awful.key({ modkey,           }, "k",
            function ()
                awful.client.focus.byidx(-1)
            end,
            {description = "focus previous by index", group = "client"}
        ),

        -- Layout manipulation
        awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
                {description = "swap with next client by index", group = "client"}),
        awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
                {description = "swap with previous client by index", group = "client"}),
        awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
                {description = "focus the next screen", group = "screen"}),
        awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
                {description = "focus the previous screen", group = "screen"}),
        awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
                {description = "jump to urgent client", group = "client"}),
        awful.key({ modkey,           }, "Tab",
            function ()
                awful.client.focus.history.previous()
                if client.focus then
                    client.focus:raise()
                end
            end,
            {description = "go back", group = "client"}),

        -- Standard program
        awful.key({ modkey, "Control" }, "r", awesome.restart,
                {description = "reload awesome", group = "awesome"}),
        awful.key({ modkey, "Shift"   }, "q", logout_popup.launch,
                {description = "quit awesome", group = "awesome"}),
        -- awful.key({ modkey, "Shift"   }, "q", awesome.quit,
        --         {description = "quit awesome", group = "awesome"}),

        awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
                {description = "increase master width factor", group = "layout"}),
        awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
                {description = "decrease master width factor", group = "layout"}),
        awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
                {description = "increase the number of master clients", group = "layout"}),
        awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
                {description = "decrease the number of master clients", group = "layout"}),
        awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
                {description = "increase the number of columns", group = "layout"}),
        awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
                {description = "decrease the number of columns", group = "layout"}),
        awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
                {description = "select next", group = "layout"}),
        awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
                {description = "select previous", group = "layout"}),

        awful.key({ modkey, "Control" }, "n",
                function ()
                    local c = awful.client.restore()
                    -- Focus restored client
                    if c then
                        c:emit_signal(
                            "request::activate", "key.unminimize", {raise = true}
                        )
                    end
                end,
                {description = "restore minimized", group = "client"}),
        awful.key({ modkey, "Shift" }, "n",
                    function ()
                        local c = awful.client.restore()
                        while c do
                            c:emit_signal(
                                "request::activate",
                                "key.unminimize",
                                {raise = true}
                            )
                            c = awful.client.restore()
                        end
                    end,
                  {description = "restore all minimized", group = "client"}),
        -- Prompt
        awful.key({ modkey },            "r",     function () awful.spawn('dmenu_run -c -bw 5 -l 20') end,
                {description = "run prompt", group = "launcher"}),

        awful.key({ modkey }, "x",
                function ()
                    awful.prompt.run {
                        prompt       = "Run Lua code: ",
                        textbox      = awful.screen.focused().mypromptbox.widget,
                        exe_callback = awful.util.eval,
                        history_path = awful.util.get_cache_dir() .. "/history_eval"
                    }
                end,
                {description = "lua execute prompt", group = "awesome"}),
        -- Volume
        awful.key({ modkey }, "]", function() volume_widget:inc(5) end,
                {description="increase volume (5)", group="volume"}),
        awful.key({ modkey, "Shift" }, "]", function() volume_widget:inc(1) end,
                {description="increase volume (1)", group="volume"}),
        awful.key({ modkey }, "[", function() volume_widget:dec(5) end,
                {description="decrease volume (5)", group="volume"}),
        awful.key({ modkey, "Shift" }, "[", function() volume_widget:dec(1) end,
                {description="decrease volume (1)", group="volume"}),
        awful.key({ modkey }, "\\", function() volume_widget:toggle() end,
                {description="mute volume", group="volume"}),
        -- Apps
        awful.key({ modkey, "Shift" }, "e",      function() awful.spawn('emacs') end,
                {description="emacs", group="launcher"}),
        awful.key({ modkey, "Shift" }, "b",      function() awful.spawn(browser) end,
                {description=browser, group="launcher"}),
        awful.key({ modkey, "Shift" }, "Return", function () awful.spawn(terminal) end,
                {description = "open a terminal", group = "launcher"})
    )
end

local function init_client_keys(vars)
    local modkey = vars.modkey
    return gears.table.join(
        awful.key({ modkey,           }, "f",
            function (c)
                c.fullscreen = not c.fullscreen
                c:raise()
            end,
            {description = "toggle fullscreen", group = "client"}),
        awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
                {description = "close", group = "client"}),
        awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
                {description = "toggle floating", group = "client"}),
        awful.key({ modkey, }, "Return", function (c) c:swap(awful.client.getmaster()) end,
                {description = "move to master", group = "client"}),
        awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
                {description = "move to screen", group = "client"}),
        awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
                {description = "toggle keep on top", group = "client"}),
        awful.key({ modkey,           }, "n",
            function (c)
                -- The client currently has the input focus, so it cannot be
                -- minimized, since minimized clients can't have the focus.
                c.minimized = true
            end ,
            {description = "minimize", group = "client"}),
        awful.key({ modkey,           }, "m",
            function (c)
                c.maximized = not c.maximized
                c:raise()
            end ,
            {description = "(un)maximize", group = "client"})
    )
end

local function init_tag_keybindings(vars, globalkeys)
    local modkey = vars.modkey
    local tags = vars.tags
    for i = 1, 9 do
        globalkeys = gears.table.join(globalkeys,
            -- View tag only.
            awful.key({ modkey }, "#" .. i + 9,
                    function ()
                            local screen = awful.screen.focused()
                            local tag = tags[i]
                            if tag then
                            sharedtags.viewonly(tag, screen)
                            end
                    end,
                    {description = "view tag #"..i, group = "tag"}),
            -- Toggle tag display.
            awful.key({ modkey, "Control" }, "#" .. i + 9,
                    function ()
                        local screen = awful.screen.focused()
                        local tag = tags[i]
                        if tag then
                            sharedtags.viewtoggle(tag, screen)
                        end
                    end,
                    {description = "toggle tag #" .. i, group = "tag"}),
            -- Move client to tag.
            awful.key({ modkey, "Shift" }, "#" .. i + 9,
                    function ()
                        if client.focus then
                            local tag = tags[i]
                            if tag then
                                client.focus:move_to_tag(tag)
                            end
                        end
                    end,
                    {description = "move focused client to tag #"..i, group = "tag"}),
            -- Toggle tag on focused client.
            awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                    function ()
                        if client.focus then
                            local tag = tags[i]
                            if tag then
                                client.focus:toggle_tag(tag)
                            end
                        end
                    end,
                    {description = "toggle focused client on tag #" .. i, group = "tag"})
        )
    end
    return globalkeys
end

local function init(vars)
    local globalkeys = init_global_keys(vars)
    local clientkeys = init_client_keys(vars)
    globalkeys = init_tag_keybindings(vars, globalkeys)
    return {global = globalkeys, client = clientkeys}
end

keybindings.init = init
return keybindings
