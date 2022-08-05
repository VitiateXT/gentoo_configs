#!/bin/bash

runuser -u moritz amixer set Master 3%+

logger "[ACPI] volumeup"
