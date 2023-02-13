local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")

local logout_popup = require("awesome-wm-widgets.logout-popup-widget.logout-popup")
local calendar_widget = require("awesome-wm-widgets.calendar-widget.calendar")
local battery_widget = require("awesome-wm-widgets.battery-widget.battery")
local volume_widget = require('awesome-wm-widgets.volume-widget.volume')

local wibar = {}

function make_separator()
    return wibox.widget.separator({
        orientation = 'vertical',
        forced_width = 5,
        color = '#005577',
        span_ratio = 0.8})
end

local function make_textclock()
    local tc = wibox.widget.textclock()
    local cw = calendar_widget({placement = 'top_right', theme = 'nord'})
    tc:connect_signal("button::press",
        function(_, _, _, button)
            if button == 1 then cw.toggle() end
        end)
    return tc
end

local function make_tasklist_buttons()
    return gears.table.join(
        awful.button({ }, 1, function (c)
                if c == client.focus then
                    c.minimized = true
                else
                    c:emit_signal(
                    "request::activate",
                    "tasklist",
                    {raise = true}
                    )
                end
            end),
        awful.button({ }, 3, function()
                awful.menu.client_list({ theme = { width = 250 } })
            end),
        awful.button({ }, 4, function ()
                awful.client.focus.byidx(1)
            end),
        awful.button({ }, 5, function ()
                awful.client.focus.byidx(-1)
            end))

end

local function make_taglist_buttons()
    return gears.table.join(
        awful.button({ }, 1, function(t) t:view_only() end),
        awful.button({ modkey }, 1, function(t)
                if client.focus then
                    client.focus:move_to_tag(t)
                end
            end),
        awful.button({ }, 3, awful.tag.viewtoggle),
        awful.button({ modkey }, 3, function(t)
                if client.focus then
                    client.focus:toggle_tag(t)
                end
            end),
        awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
        awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end))
end

local function make_left_widgets(s)
    local left_widgets = wibox.layout.fixed.horizontal()
    left_widgets:add(s.mypromptbox)
    left_widgets:add(s.mylayoutbox)
    left_widgets:add(make_separator())
    left_widgets:add(s.mytaglist)
    left_widgets:add(make_separator())
    return left_widgets
end

local function make_right_widgets(s)
    local right_widgets = wibox.layout.fixed.horizontal()
    right_widgets:add(make_separator())
    if s == screen.primary then
        right_widgets:add(wibox.widget.systray())
        right_widgets:add(make_separator())
    end
    right_widgets:add(volume_widget())
    local h = io.popen("acpi")
    if h:read(1) then
        right_widgets:add(make_separator())
        right_widgets:add(battery_widget({
            path_to_icons = ".config/awesome/awesome-wm-widgets/battery-widget/",
            display_notification = true
        }))
    end
    h:close()
    right_widgets:add(make_separator())
    right_widgets:add(make_textclock())
    right_widgets:add(make_separator())
    right_widgets:add(logout_popup.widget{})
    return right_widgets
end

local function make_wibar(s)
    local left_widgets = make_left_widgets(s)
    local right_widgets = make_right_widgets(s)
    local layout = wibox.layout.align.horizontal(
        left_widgets,
        s.mytasklist,
        right_widgets)
    return awful.wibar({
        position = "top",
        screen = s,
        widget = layout })
end

local function init()
    awful.screen.connect_for_each_screen(function(s)
            -- Create a promptbox for each screen
            s.mypromptbox = awful.widget.prompt()
            -- We need one layoutbox per screen.
            s.mylayoutbox = awful.widget.layoutbox(s)
            s.mylayoutbox:buttons(gears.table.join(
                awful.button({ }, 1, function () awful.layout.inc( 1) end),
                awful.button({ }, 3, function () awful.layout.inc(-1) end),
                awful.button({ }, 4, function () awful.layout.inc( 1) end),
                awful.button({ }, 5, function () awful.layout.inc(-1) end)))
            -- Create a taglist widget
            s.mytaglist = awful.widget.taglist {
                screen  = s,
                filter  = awful.widget.taglist.filter.all,
                buttons = make_taglist_buttons(),
                style = { bg_focus = "#005577" }
            }
            -- Create a tasklist widget
            s.mytasklist = awful.widget.tasklist {
                screen  = s,
                filter  = awful.widget.tasklist.filter.currenttags,
                buttons = make_tasklist_buttons(),
                style = { bg_focus = "#005577" }
            }

            s.mywibox = make_wibar(s)
        end)
end

wibar.init = init
wibar.logout_popup = logout_popup

return wibar
