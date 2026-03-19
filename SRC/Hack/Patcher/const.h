#ifndef CONST_H
#define CONST_H

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_mixer.h>

#include "type.h"

// -------------------------------------------------------------------------------------------------------

static const char *NAME_PROGRAM_FILE    = "PSW.asm";
static const char *NAME_FONT_FILE       = "font.ttf";
static const char *FONE_MUSIC           = "music.mp3";
static const int SCREEN_WIDTH   = 400;
static const int SCREEN_HEIGHT  = 600;

static const SDL_Color FONT_COLOR = {255, 255, 255, 255};
static const SDL_Color BUTTON_PR_COLOR = {82, 255, 151, 255};
static const SDL_Color BUTTON_UN_COLOR = {255, 82, 243, 255};
static const SDL_Color TEXT_COLOR = {16, 0, 192, 255};
// rgb(16, 0, 192)

static const int BUTTON_WIDTH = 100;
static const int BUTTON_HEIGHT = 40;

static const SDL_Rect BUTTON_PLACES[] = {
    {.x = 150, .y = 400, .w = 100, .h = BUTTON_HEIGHT},
    {.x = 50, .y = 450, .w = 300, .h = BUTTON_HEIGHT},
    {.x = 50, .y = 400,  .w = BUTTON_WIDTH, .h = BUTTON_HEIGHT},
    {.x = 250, .y = 400, .w = BUTTON_WIDTH, .h = BUTTON_HEIGHT},
    {.x = 50, .y = 200, .w = 300, .h = 100},
};
static const char *BUTTON_TEXT[] = {
    "I AGREE",
    "NO, NO, NO MR. FISH",
    "LEAVE",
    "PATCH",
    "START/STOP MUSIC",
};
static const int AMOUNT_BUTTONS = 5;

static const SDL_Rect TEXT_PLACE[] = {
    {.x = 20, .y = 100, .w = 360, .h = 260},
    {.x = 100, .y = 50, .w = 200, .h = 100}
};
static const char *TEXT_TEXT[] = {
    "USAGE POLICY\n" \
    "To use the patcher, you\n must accept the \nfollowing agreement:\n" \
    "1. You must pet Poltorashka\n at least 3 times a day\n" \
    "2. You should wash yourself \nat least once a day",
    "Successful patch",
    "Already patched",
    "Unknown file",
};

static const int VOLUME_FONE_MUSIC = MIX_MAX_VOLUME / 4;

#define _BUTTON_AGREE 0
#define _BUTTON_NOT_AGREE 1
#define _BUTTON_EXIT 0
#define _BUTTON_PATCH 1
#define _BUTTON_MUSIC 2

#define _TEXT_POLICY 2
#define _TEXT_SUCCESS 3

#define _MENU_INIT 0
#define _MENU_NOT_PATCHED 1
#define _MENU_PATCHED 2
#define _MENU_ALREADY_PATCHED 3
#define _MENU_UNKNOWN_FILE 4

static const hash_t NOT_PATCHED_FILE_HASH = {0, 0, 0, 0};
static const hash_t PATCHED_FILE_HASH = {0, 0, 0, 0};


#endif /* CONST_H */
