`keyboards/dactyl_manuform/5x6/info.json`:

```json
{
    "keyboard_name": "Dactyl-Manuform (5x6)",
    "manufacturer": "tshort",
    "url": "",
    "maintainer": "qmk",
    "usb": {
        "vid": "0x444D",
        "pid": "0x3536",
        "device_version": "0.0.1"
    },
    "ws2812": {
        "pin": "D3"
    },
    "matrix_pins": {
        "cols": ["A8", "A9", "A10", "A11", "A12", "A15"],
        "rows": ["A7", "A6", "A5", "A4", "A3", "A2"]
    },
    "diode_direction": "ROW2COL",
    "split": {
        "soft_serial_pin": "B6"
    },
    "processor": "STM32F103",
    "bootloader": "stm32duino",
    "layouts": {
        "LAYOUT_5x6": {
            "layout": [
                {"matrix": [0, 0], "x": 0, "y": 0},
                {"matrix": [0, 1], "x": 1, "y": 0},
                {"matrix": [0, 2], "x": 2, "y": 0},
                {"matrix": [0, 3], "x": 3, "y": 0},
                {"matrix": [0, 4], "x": 4, "y": 0},
                {"matrix": [0, 5], "x": 5, "y": 0},

                {"matrix": [6, 0], "x": 11, "y": 0},
                {"matrix": [6, 1], "x": 12, "y": 0},
                {"matrix": [6, 2], "x": 13, "y": 0},
                {"matrix": [6, 3], "x": 14, "y": 0},
                {"matrix": [6, 4], "x": 15, "y": 0},
                {"matrix": [6, 5], "x": 16, "y": 0},

                {"matrix": [1, 0], "x": 0, "y": 1},
                {"matrix": [1, 1], "x": 1, "y": 1},
                {"matrix": [1, 2], "x": 2, "y": 1},
                {"matrix": [1, 3], "x": 3, "y": 1},
                {"matrix": [1, 4], "x": 4, "y": 1},
                {"matrix": [1, 5], "x": 5, "y": 1},

                {"matrix": [7, 0], "x": 11, "y": 1},
                {"matrix": [7, 1], "x": 12, "y": 1},
                {"matrix": [7, 2], "x": 13, "y": 1},
                {"matrix": [7, 3], "x": 14, "y": 1},
                {"matrix": [7, 4], "x": 15, "y": 1},
                {"matrix": [7, 5], "x": 16, "y": 1},

                {"matrix": [2, 0], "x": 0, "y": 2},
                {"matrix": [2, 1], "x": 1, "y": 2},
                {"matrix": [2, 2], "x": 2, "y": 2},
                {"matrix": [2, 3], "x": 3, "y": 2},
                {"matrix": [2, 4], "x": 4, "y": 2},
                {"matrix": [2, 5], "x": 5, "y": 2},

                {"matrix": [8, 0], "x": 11, "y": 2},
                {"matrix": [8, 1], "x": 12, "y": 2},
                {"matrix": [8, 2], "x": 13, "y": 2},
                {"matrix": [8, 3], "x": 14, "y": 2},
                {"matrix": [8, 4], "x": 15, "y": 2},
                {"matrix": [8, 5], "x": 16, "y": 2},

                {"matrix": [3, 0], "x": 0, "y": 3},
                {"matrix": [3, 1], "x": 1, "y": 3},
                {"matrix": [3, 2], "x": 2, "y": 3},
                {"matrix": [3, 3], "x": 3, "y": 3},
                {"matrix": [3, 4], "x": 4, "y": 3},
                {"matrix": [3, 5], "x": 5, "y": 3},

                {"matrix": [9, 0], "x": 11, "y": 3},
                {"matrix": [9, 1], "x": 12, "y": 3},
                {"matrix": [9, 2], "x": 13, "y": 3},
                {"matrix": [9, 3], "x": 14, "y": 3},
                {"matrix": [9, 4], "x": 15, "y": 3},
                {"matrix": [9, 5], "x": 16, "y": 3},

                {"matrix": [4, 2], "x": 2, "y": 4},
                {"matrix": [4, 3], "x": 3, "y": 4},

                {"matrix": [10, 2], "x": 13, "y": 4},
                {"matrix": [10, 3], "x": 14, "y": 4},

                {"matrix": [4, 4], "x": 4, "y": 5},
                {"matrix": [4, 5], "x": 5, "y": 5},

                {"matrix": [10, 0], "x": 11, "y": 5},
                {"matrix": [10, 1], "x": 12, "y": 5},

                {"matrix": [5, 4], "x": 6, "y": 6},
                {"matrix": [5, 5], "x": 7, "y": 6},

                {"matrix": [11, 0], "x": 9, "y": 6},
                {"matrix": [11, 1], "x": 10, "y": 6},

                {"matrix": [5, 2], "x": 6, "y": 7},
                {"matrix": [5, 3], "x": 7, "y": 7},

                {"matrix": [11, 2], "x": 9, "y": 7},
                {"matrix": [11, 3], "x": 10, "y": 7}
            ]
        }
    }
}
```

change `rules.mk`:

```
# Build Options
#   change yes to no to disable
#
BOOTMAGIC_ENABLE = no       # Enable Bootmagic Lite
MOUSEKEY_ENABLE = yes       # Mouse keys
EXTRAKEY_ENABLE = yes       # Audio control and System control
CONSOLE_ENABLE = no         # Console for debug
COMMAND_ENABLE = yes        # Commands for debug and configuration
NKRO_ENABLE = no            # Enable N-Key Rollover
BACKLIGHT_ENABLE = no       # Enable keyboard backlight functionality
RGBLIGHT_ENABLE = no        # Enable keyboard RGB underglow
AUDIO_ENABLE = no           # Audio output
SPLIT_KEYBOARD = yes
CONVERT_TO = proton_c
```

run `qmk compile`:

```
$ qmk compile -kb handwired/dactyl_manuform/5x6 -km default
```

getting an error

```
Ψ Compiling keymap with make --jobs=1 handwired/dactyl_manuform/5x6:default


QMK Firmware 0.18.17
Making handwired/dactyl_manuform/5x6 with keymap default

platforms/chibios/platform.mk:143: lib/chibios-contrib/os/hal/boards/GENERIC_STM32_F103/board.mk: No such file or directory
make[1]: *** No rule to make target 'lib/chibios-contrib/os/hal/boards/GENERIC_STM32_F103/board.mk'.  Stop.
make: *** [Makefile:392: handwired/dactyl_manuform/5x6:default] Error 1
Make finished with errors
(failed reverse-i-search)`git submodule': ^Ct st
```

apparently, [need to set `bootloader` to `stm32duino`](https://github.com/qmk/qmk_firmware/issues/10979#issuecomment-731659381):

> The difference right now is that boards using MCU = STM32F103 are setting BOOTLOADER = stm32duino or providing their own board/linker files.

getting another error:

```
arm-none-eabi-gcc (15:9-2019-q4-0ubuntu1) 9.2.1 20191025 (release) [ARM/arm-9-branch revision 277599]
Copyright (C) 2019 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Size before:
   text    data     bss     dec     hex filename
      0   19462       0   19462    4c06 handwired_dactyl_manuform_5x6_default.bin

Compiling: .build/obj_handwired_dactyl_manuform_5x6/src/default_keyboard.c                         In file included from
 ./platforms/chibios/drivers/wear_leveling/wear_leveling_efl_config.h:6,
                 from <command-line>:
./lib/chibios/os/hal/include/hal.h:130:2: error: #error "obsolete or unknown configuration file"
  130 | #error "obsolete or unknown configuration file"
      |  ^~~~~
 [ERRORS]
 |
 |
 |
make[1]: *** [builddefs/common_rules.mk:361: .build/obj_handwired_dactyl_manuform_5x6_default/.build/obj_handwired_dactyl_manuform_5x6/src/default_keyboard.o] Error 1
make: *** [Makefile:392: handwired/dactyl_manuform/5x6:default] Error 1
Make finished with errors
```

SSH keys permissions, JFYI:

* `.ssh` directory: `700 (drwx------)`
* public key (`.pub` files): `644 (-rw-r--r--)`
* private key (`id_rsa`): `600 (-rw-------)`
