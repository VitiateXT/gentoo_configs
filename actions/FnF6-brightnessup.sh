#!/bin/bash

IncVal=20

read -r MaxVal < "sys/class/backlight/amdgpu_bl0/max_brightness"

read -r CurrVal < "/sys/class/backlight/amdgpu_bl0/brightness"

NewVal=$(($CurrVal + $IncVal));
echo $NewVal

ThresholdVal=$(($NewVal<$MaxVal?$NewVal:$MaxVal))
echo $ThresholdVal

echo -n $ThresholdVal > /sys/class/backlight/amdgpu_bl0/brightness

logger "[ACPI] brightnessup |$CurrVal| |$NewVal| |$ThresholdVal|"
