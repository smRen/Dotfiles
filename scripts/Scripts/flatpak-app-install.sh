#!/bin/bash

main() {
  FLATPAK_APPS=$(<'flatpak-apps.txt')
  if ! [[ -x "$(command -v flatpak)" ]]; then
          printf "Flatpak not installed"
          return 1
  fi

  flatpak --user remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo

  for APP in $FLATPAK_APPS; do
      flatpak --user install "$APP" --assumeyes
  done

  return 0
}

main
