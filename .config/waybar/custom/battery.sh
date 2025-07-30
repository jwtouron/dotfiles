#!/bin/sh

acpi_output="$(acpi | head -n1)"

tooltip="$acpi_output"
text=""
warning_threshold=30
critical_threshold=15
class=battery

# if ! echo -n "$acpi_output" | grep -q Full; then
    # text="$(echo -n "$acpi_output" | awk -F', ' '{print "🔋  "$2}')"
# fi

# echo -n "{\"text\": \"$text\", \"tooltip\": \"$tooltip\", \"class\": \"$class\"}"
echo -n "{\"text\": \"\"}"
