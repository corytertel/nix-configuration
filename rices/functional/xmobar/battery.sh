#!/bin/sh

# original script was taken from Figo, modified since then

battery=('<fc=#e60909></fc>' '<fc=#e60909></fc>' '<fc=#e60909></fc>' '<fc=#ed8f23></fc>' '<fc=#ed8f23></fc>' '<fc=#ed8f23></fc>'    )
charging_battery=('<fc=#e60909></fc>' '<fc=#e60909></fc>' '<fc=#e60909></fc>' '<fc=#ed8f23></fc>' '<fc=#ed8f23></fc>' '<fc=#ed8f23></fc>' '<fc=#1f8c35></fc>' '<fc=#1f8c35></fc>' '<fc=#1f8c35></fc>' '<fc=#1f8c35></fc>')

Percentage=($(acpi -b | rg 'Battery 0' | awk -F'[,%[:space:]]+' '{ printf "%s %s", $3, $4 }'))
Status=${Percentage[0]}

Percentage=${Percentage/\%,/}
Percentage=${Percentage[1]}
Percentage="${Percentage-1}"
Percentage=${Percentage:0:1}


if [ "$Status" = "Discharging" ]; then
    echo ${battery[Percentage]}
else
    echo ${charging_battery[Percentage]}
fi
