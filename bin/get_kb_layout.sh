#!/bin/bash

case "$(xset -q | grep -A 0 'LED' | cut -c59-67)" in
  "00000000") KBD="English" ;;
  "00001000") KBD="Farsi" ;;
  *) KBD="unknown" ;;
esac

echo $KBD
