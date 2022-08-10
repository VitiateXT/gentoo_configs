#!/bin/bash

DecVal=5

MinVal=0

read -r CurrVal < "/sys/class/backlight/amdgpu_bl0/brightness"

NewVal=$(($CurrVal -$DecVal));
echo $NewVal

ThresholdVal=$(($NewVal>$MinVal?$NewVal:$MinVal))
echo $ThresholdVal

echo -n $ThresholdVal > /sys/class/backlight/amdgpu_bl0/brightness

logger "[ACPI] brightnessdown |$CurrVal| |$NewVal| |$ThresholdVal|"
