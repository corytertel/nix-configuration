#!/bin/sh

# original script was taken from Figo, modified since then

battery=('đĒĢ' 'đĒĢ' 'đ' 'đ' 'đ' 'đ' 'đ' 'đ' 'đ' 'đ')
charging_battery=('đĒĢâĄ' 'đĒĢâĄ' 'đâĄ' 'đâĄ' 'đâĄ' 'đâĄ' 'đâĄ' 'đâĄ' 'đâĄ' 'đâĄ')

Percentage=($(acpi -b | rg 'Battery 0' | awk -F'[,%[:space:]]+' '{ printf "%s %s", $3, $4 }'))
Status=${Percentage[0]}
NumPercentage=${Percentage[1]}

Percentage=${Percentage/\%,/}
Percentage=${Percentage[1]}
Percentage="${Percentage-1}"
Percentage=${Percentage:0:1}


if [ "$Status" = "Discharging" ]; then
    echo -e "${battery[Percentage]}\n$NumPercentage%"
else
    echo -e "${charging_battery[Percentage]}\n$NumPercentage%"
fi
